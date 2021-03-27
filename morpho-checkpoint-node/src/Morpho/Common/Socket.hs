module Morpho.Common.Socket
  ( nodeAddressInfo,
  )
where

import Cardano.Prelude
import Morpho.Config.Types
import Network.Socket as Socket
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
