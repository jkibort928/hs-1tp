{-# LANGUAGE MultiWayIf #-}
module EphemHttps ( ephemServe ) where

-- Library Imports
import Data.Int
import Data.Time
import Control.Monad ( unless )
import System.Directory ( getFileSize, canonicalizePath )
import System.Posix.Files ( fileAccess )
import System.Timeout ( timeout )
import Network.Socket ( Socket, SockAddr )
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC ( pack, unpack )
import qualified Data.ByteString.Char8 as BSLC ( toStrict )

import Version (serverHeader)

bufferSize :: Int
bufferSize = 1024

chunkSize :: Int64 -- ByteString.Lazy ( splitAt ) needs it to be this way 
chunkSize = 4096

maxHeaderLength :: Int
maxHeaderLength = 16384

-- Header recv timeout (in microseconds)
headerTimeout :: Int
headerTimeout = 10000000 -- 10 seconds to send entire header

-- Chunk send timeout (in microseconds)
sendTimeout :: Int
sendTimeout = 30000000 -- 30 seconds to receive a chunk

------- Configuration ---------
supportedMethods :: [String]
supportedMethods = ["GET", "HEAD"]

--- HTTP Error Status Codes ---

send403 :: Socket -> IO ()
send403 sock = sendAll sock $ BSC.pack "HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n"

send404 :: Socket -> IO ()
send404 sock = sendAll sock $ BSC.pack "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"

send405 :: Socket -> IO ()
send405 sock = sendAll sock $ BSC.pack "HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n"

send505 :: Socket -> IO ()
send505 sock = sendAll sock $ BSC.pack "HTTP/1.1 505 HTTP Version Not Supported\r\nContent-Length: 0\r\n\r\n"

---------- Helpers ------------

-- Format as "YYYY-MM-DD HH:MM:SS"
getTimeStamp :: IO String
getTimeStamp = formatTime defaultTimeLocale "%F %T" <$> getZonedTime
-- %F = %Y-%m-%d, %T = %H:%M:%S

-- Splits a ByteString immediately after the first occurrence of a delimiter.
-- The delimiter is kept on the left side of the split.
splitAfter :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
splitAfter delim buff = case BS.breakSubstring delim buff of
    (before, matchAndAfter)
        | BS.null matchAndAfter -> (buff, BS.empty)
        | otherwise             -> 
            let (match, after) = BS.splitAt (BS.length delim) matchAndAfter
            in (before `BS.append` match, after)

-- Returns the raw request head
readRequest :: Socket -> IO (BS.ByteString, BS.ByteString)
readRequest sock = do
    result <- timeout headerTimeout (getHeaders BS.empty)
    case result of
        Nothing -> return BS.empty -- Timeout occured (slowloris protection)
        Just (finalHdr, _) -> return finalHdr -- Throw away body, just take the final header
    where
        delim = BSC.pack "\r\n\r\n"
        overlapSz = (BS.length delim) - 1 -- A delim can only be fragmented with a max of n-1 on each side
        
        getHeaders :: BS.ByteString -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
        getHeaders acc chunk = do
            -- Take the overlap from the accumulated and treat it as incoming to avoid delim being split
            let overlap = BS.takeEnd overlapSz acc
            let acc' = BS.dropEnd overlapSz acc
            let chunk' = overlap `BS.append` chunk

            -- Try to split the delim from the last chunk
            let (finalPart, rest) = splitAfter delim chunk'
            if delim `BS.isSuffixOf` finalPart then do
                -- Delim successfully split off, finalize
                let finalHeader = acc' `BS.append` finalPart
                if BS.length finalHeader > maxHeaderLength
                    then return (BS.empty, BS.empty) -- Length exceeded, force a 400 error
                    else return (finalHeader, rest)
            else do
                -- Delim not found yet, recev more
                nextChunk <- recv sock bufferSize
                if BS.null nextChunk then return (acc' `BS.append` chunk', BS.empty)
                else do
                    let nextAcc = acc' `BS.append` chunk'
                    if BS.length nextAcc > maxHeaderLength
                        then return (BS.empty, BS.empty) -- Length exceeded, force a 400 error
                        else getHeaders nextAcc nextChunk

    
-- Returns (method, filepath)
-- Empty method string signifies an error has already been sent to the client
httpDecode :: Socket -> IO (String, String)
httpDecode sock = do
    request <- readRequest sock
    
    let reqLine = BSC.unpack $ fst $ BS.breakSubstring (BSC.pack "\r\n") request
    let (method, rawUri, httpVer) = unpackReqLine reqLine
    
    --putStrLn ("FULL REQUEST:\n" ++ show request)
    --putStrLn "----------------------------------"
    --putStrLn ("Request line: " ++ reqLine)
    --putStrLn ("method: " ++ method ++ "\nrawUri: " ++ rawUri ++ "\nhttpVer: " ++ httpVer)

    if  | BS.null request                               -> return ("", "", BS.empty)
        | httpVer `notElem` ["HTTP/1.1", "HTTP/1.0"]    -> failWith (send505 sock)
        | method `notElem` supportedMethods             -> failWith (send405 sock)
        | otherwise                                     -> return (method, rawUri)

    where
        -- Does IO action then returns empty
        failWith :: IO () -> IO (String, String)
        failWith errAction = errAction >> return ("", "")
    
        -- Breaks up the request line by spaces into a triple
        unpackReqLine :: String -> (String, String, String)
        unpackReqLine str = (fst split1, fst split2, drop 1 $ snd split2)
            where 
                split1 = break (' '==) str
                split2 = break (' '==) (drop 1 $ snd split1)

-- Sends the requested file, crafting the HTTP request
sendFile :: Bool -> String -> Socket -> IO ()
sendFile isHead filePath sock = do

    hasAccess <- fileAccess filePath True False False

    -- Check file access 
    if not hasAccess then do
        -- Send 403 forbidden, cannot read file
        send403 sock
    else do

        -- Resolve symlinks for the true size of the file
        canonPath <- canonicalizePath filePath
        fileSize <- getFileSize canonPath

        let header = BSC.pack $ "HTTP/1.1 200 OK\r\n" ++
                                serverHeader ++
                                "Connection: close\r\n" ++
                                "Content-Length: " ++ (show fileSize) ++ "\r\n" ++
                                "Content-Type: application/octet-stream\r\n" ++ -- Force file download, prevent display
                                "X-Content-Type-Options: nosniff\r\n" ++
                                "\r\n"
        sendAll sock header

        unless isHead $ do
            fileContents <- BSL.readFile filePath
            sendChunks sock fileContents
            
    where
        sendChunks :: Socket -> BSL.ByteString -> IO ()
        sendChunks sock' content = do
            let (chunk, rest) = BSL.splitAt chunkSize content -- Split the content into chunkSize sized chunks
            unless (BSL.null chunk) $ do -- Stop if we ran out
                -- Wrap the send in a timeout ("slow read" attack mitigation)
                result <- timeout sendTimeout $ sendAll sock' (BSLC.toStrict chunk) -- Convert the chunk to strict and send it
                case result of
                    Nothing -> return () -- Client stopped reading (read too slowly), exit loop
                    Just () -> sendChunks sock' rest -- No timeout, continue chunks

---------- Exported -----------

ephemServe :: String -> String -> Socket -> SockAddr -> IO Bool
ephemServe filePath expectedID sock cliAddr = do
    (method, path) <- httpDecode sock
    
    if null method
        then return True -- Error sent, keep listening
        else do
    	    timestamp <- getTimeStamp
    	    putStrLn (timestamp ++ " " ++ show cliAddr ++ ": " ++ method ++ " " ++ path)

            if path /= ("/" ++ expectedID)
                then do
                     -- ID mismatch, obscure with 404 and keep listening
                    send404 sock
                    return True
                else do
                    -- ID match, send the file, terminate (return False) if not head
                    let isHead = method == "HEAD"
                    sendFile isHead filePath sock
                    return isHead
