{-# LANGUAGE ScopedTypeVariables #-}
module Connection ( Connection(..), plainConnection, secureConnection ) where

import Network.Socket ( Socket, gracefulClose )
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Exception as E
import Network.TLS ( Context, recvData, sendData, bye )

-- Connection close timeout (in milliseconds)
closeTimeout :: Int
closeTimeout = 5000

-- The abstracted connection interface
data Connection = Connection {
    connRecv  :: Int -> IO BS.ByteString
 ,  connSend  :: BS.ByteString -> IO ()
 ,  connClose :: IO ()
}

-- Initialize a plaintext HTTP connection
plainConnection :: Socket -> Connection
plainConnection sock = Connection {
    connRecv  = recv sock
  , connSend  = sendAll sock
  , connClose = gracefulClose sock closeTimeout
}

-- Initialize a TLS encrypted HTTPS connection
secureConnection :: Context -> Socket -> Connection
secureConnection ctx sock = Connection {
    connRecv  = \_ -> recvData ctx
  , connSend  = \bs -> sendData ctx (BSL.fromStrict bs)
  , connClose = do
      -- Try to send the TLS close_notify alert cleanly
      _ <- E.try (bye ctx) :: IO (Either E.SomeException ())
      -- Regardless of TLS success/failure, gracefully close the raw socket
      gracefulClose sock closeTimeout
}
