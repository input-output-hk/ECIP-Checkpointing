module Morpho.Common.Socket
  ( nodeAddressInfo,
    removeStaleLocalSocket,
  )
where

import Cardano.Prelude
import Morpho.Config.Types
import Network.Socket as Socket
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import qualified Prelude

nodeAddressInfo :: NodeAddress -> IO [AddrInfo]
nodeAddressInfo (NodeAddress hostAddr port) =
  -- TODO handle errors
  getAddrInfo
    (Just hints)
    (fmap Prelude.show $ unNodeHostAddress hostAddr)
    (Just $ Prelude.show port)
  where
    hints =
      defaultHints
        { addrFlags = [AI_PASSIVE, AI_ADDRCONFIG],
          addrSocketType = Stream
        }

-- | Remove the socket file
removeStaleLocalSocket :: FilePath -> IO ()
removeStaleLocalSocket socketFp = do
  removeFile socketFp
    `catch` \e ->
      if isDoesNotExistError e
        then return ()
        else throwIO e
