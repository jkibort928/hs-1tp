module Version ( serverVersion, serverHeader ) where

import Data.Version (showVersion)
import Paths_hs_1tp (version) -- Magic auto-generated module by cabal

serverVersion :: String
serverVersion = showVersion version

-- Pre-formatted HTTP header line
serverHeader :: String
serverHeader =  "Server: hs-1tp/" ++ serverVersion ++ "\r\n" ++
                "X-Origin-Server: hs-1tp/" ++ serverVersion ++ "\r\n" ++
                "X-Powered-By: Haskell :D\r\n"
