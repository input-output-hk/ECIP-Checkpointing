module Morpho.Ledger.CodecConfig
  ( CodecConfig (..),
    defaultCodecConfig,
  )
where

import Morpho.Ledger.Block
import Ouroboros.Consensus.Block.Abstract

{-------------------------------------------------------------------------------
  Codec Config
-------------------------------------------------------------------------------}
data instance CodecConfig MorphoBlock
  = MorphoCodecConfig
      { getMorphoSecurityParam :: !SecurityParam
      }
  deriving (Generic, NoUnexpectedThunks)

data ExtractNodeInfoError = MissingCoreNodeId | MissingNumCoreNodes

defaultSecurityParam :: SecurityParam
defaultSecurityParam = SecurityParam 5

-- TODO better place for this
defaultCodecConfig :: CodecConfig MorphoBlock
defaultCodecConfig = MorphoCodecConfig defaultSecurityParam
