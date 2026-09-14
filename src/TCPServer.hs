module TCPServer ( runServer ) where

import Network.Socket 
import Control.Monad ( when )
import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE

-- Resolve the hostname given the port
resolveSelf :: ServiceName -> IO AddrInfo
resolveSelf port = do
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE] -- Wildcard when no address given
        ,   addrSocketType = Stream -- TCP
    }
    addrList <- getAddrInfo (Just hints) Nothing (Just port)
    return (NE.head addrList) -- getAddrInfo never returns an empty list without an error

-- Open the socket on address addr and set up socket options
-- If openSocket errors, we close it
-- If success, we call setupSock on it
openMySocket :: AddrInfo -> IO Socket
openMySocket addr = E.bracketOnError (openSocket addr) close setupSock
    where
        -- Setup the socket after it is opened and set it to listen
        setupSock sock =  do
            
            -- Basic socket setup options
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded

            -- Bind? I thought this was already done when we used (openSocket addr) above?
            -- Nope, that's not how it works. You need this.
            bind sock $ addrAddress addr

            -- Put the socket in listening mode
            -- Set a high listener queue size (larger than most systems' max)
            -- to use the largest queue that the system allows
            listen sock 1024

            return sock

-- Listener accept loop
-- Recursively calls itself only if the connection handler returns True
acceptLoop :: (Socket -> SockAddr -> IO Bool) -> Socket -> IO ()
acceptLoop server sock = do
    continue <- E.bracketOnError (accept sock) (close . fst) handleConn
    when continue $ acceptLoop server sock
    where
        handleConn (conn, peer) = E.finally (server conn peer) (gracefulClose conn 5000)

-- Runs the server on the given port, using server as the main function to run for each connection
runServer :: ServiceName -> (Socket -> SockAddr -> IO Bool) -> IO ()
runServer port server = withSocketsDo $ do

    putStrLn ("Starting ephemeral server on port: " ++ show port)

    -- Resolve your address
    addr <- resolveSelf port
    
    -- Open the socket
    -- Calls close on the socket if openSocket errors
    -- Calls acceptLoop on the socket if success
    E.bracket (openMySocket addr) close (acceptLoop server)
