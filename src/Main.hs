module Main (main) where

-- Library imports
import System.Environment ( getArgs )
import System.Exit ( exitSuccess, exitFailure )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( unless, when )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan )
-- Crypto (secure ID randomization)
import Crypto.Random ( getRandomBytes )
import Data.ByteArray.Encoding ( convertToBase, Base(Base64URLUnpadded) )
import qualified Data.ByteString.Char8 as B8
import Data.ByteString ( ByteString )

-- Custom imports
import Version ( serverVersion )
import CLIUtil ( checkFlags, checkOpts, parseArgs, getOpt )
import TCPServer ( runServer )
import EphemHttps ( ephemWorker, ephemConsumer )

-- Help message to be displayed
helpMessage :: String
helpMessage = "todo"

defaultPort :: String
defaultPort = "40443"

generateSecureID :: IO String
generateSecureID = do
    -- Generate 32 bytes (256 bits) of cryptographic entropy
    bytes <- getRandomBytes 32 :: IO ByteString
    pure $ B8.unpack (convertToBase Base64URLUnpadded bytes)

-- Main
main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, opts, optArgs) = parseArgs args

    when (("h" `elem` flags) || ("help" `elem` flags)) $ do
        putStrLn helpMessage
        exitSuccess
    when ("version" `elem` flags) $ do
        putStrLn $ "hs-1tp v" ++ serverVersion
        exitSuccess
        
    when (null argv) $ do
        hPutStrLn stderr "Error: No arguments specified"
        exitFailure
    unless (checkFlags flags) $ do
        hPutStrLn stderr "Error: Invalid flag"
        exitFailure
    unless (checkOpts opts optArgs) $ do
        hPutStrLn stderr "Error: Invalid options"
        exitFailure

    let filePath = head argv
    let port = getOpt ["p", "port"] defaultPort opts optArgs
    
    secureID <- generateSecureID

    -- Initialize the message queue
    chan <- newChan

    putStrLn $ "Listening for one-time GET request at: /" ++ secureID

	-- Run network listener in the background (toss ThreadID)
	-- Server spawns workers (ephemWorker) on each connection
    _ <- forkIO $ runServer port (ephemWorker chan)

    -- Pull requests from the Chan sequentially
    -- When looping/recursion stops (successful transfer), the program stops.
    ephemConsumer filePath secureID chan
    
    putStrLn "Transfer complete, terminating..."
