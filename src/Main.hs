module Main (main) where

-- Library imports
import System.Environment ( getArgs )
import System.Exit ( exitSuccess, exitFailure )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( unless, when )
-- Crypto (secure ID randomization)
import Crypto.Random ( getRandomBytes )
import Data.ByteArray.Encoding ( convertToBase, Base(Base64URLUnpadded) )
import qualified Data.ByteString.Char8 as B8
import Data.ByteString ( ByteString )

-- Custom imports
import Version ( serverVersion )
import CLIUtil ( checkFlags, checkOpts, parseArgs, getOpt )
import TCPServer ( runServer )
import EphemHttps ( ephemServe )

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
    
    putStrLn $ "Listening for one-time GET request at: /" ++ secureID

    -- Partially apply ephemServe as the server function
    runServer port (ephemServe filePath secureID)
