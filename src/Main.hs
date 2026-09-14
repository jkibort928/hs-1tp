module Main (main) where

-- Library imports
import System.Environment ( getArgs )
import System.Exit ( exitSuccess )
import Control.Exception ( throw, Exception )
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
import SimpleHttp ( doHttp )

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

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
        
    when (null argv) $ 
        throw (Error "Error: No arguments specified")
    unless (checkFlags flags) $ 
        throw (Error "Error: Invalid flag")
    unless (checkOpts opts optArgs) $ 
        throw (Error "Error: Invalid options")    

	let filePath = head argv
    	port     = getOpt ["p", "port"] defaultPort opts optArgs
	
    
    secureID <- generateSecureID
    
    putStrLn $ "Listening for one-time GET request at: /" ++ secureID

    runServer port serverFunc
        where
            serverFunc sock cliAddr = doHttp filePath secureID sock cliAddr flags
