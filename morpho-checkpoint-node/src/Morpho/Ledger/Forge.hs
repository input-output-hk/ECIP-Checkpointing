{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The CanFoge instance is Orphan.

module Morpho.Ledger.Forge
  ( forgeMorpho,
  )
where

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Hash
import Cardano.Prelude
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as Lazy
import Morpho.Ledger.Block
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.Forge
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Ticked

instance
  forall blk h c p.
  ( HashAlgorithm h,
    blk ~ (MorphoBlock h c),
    p ~ (BlockProtocol blk),
    p ~ (Bft c),
    BftCrypto c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  CanForge (MorphoBlock h c)
  where
  forgeBlock cfg _ blNo (Ticked slotNo st) = forgeMorpho consensusCfg slotNo blNo prevHash
    where
      prevHash = pointHash . morphoTip $ morphoLedgerState st
      consensusCfg = protocolConfigConsensus $ topLevelConfigProtocol cfg

forgeMorpho ::
  forall h c.
  ( HashAlgorithm h,
    BftCrypto c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  ConsensusConfig (Bft c) ->
  -- | Current slot
  SlotNo ->
  -- | Current block number
  BlockNo ->
  -- | Previous hash
  ChainHash (MorphoBlock h c) ->
  -- | Txs to add in the block
  [GenTx (MorphoBlock h c)] ->
  -- | Proof we are slot leader
  IsLeader (Bft c) ->
  MorphoBlock h c
forgeMorpho ccfg curSlot curBlock prevHash txs _ =
  MorphoBlock
    { morphoHeader = mkMorphoHeader stdHeader bftFields,
      morphoBody = body
    }
  where
    bftFields :: BftFields c (MorphoStdHeader h c)
    bftFields = forgeBftFields ccfg $ stdHeader
    body :: MorphoBody
    body =
      MorphoBody
        { morphoTxs = toMorphoBlockTx <$> txs
        }
    toMorphoBlockTx (MorphoGenTx tx txId) = MorphoBlockTx tx txId
    stdHeader :: MorphoStdHeader h c
    stdHeader =
      MorphoStdHeader
        { morphoPrev = prevHash,
          morphoSlotNo = curSlot,
          morphoBlockNo = curBlock,
          morphoBodyHash = hash body,
          morphoBlockSize = bodySize
        }
    -- We use the size of the body, not of the whole block (= header + body),
    -- since the header size is fixed and this size is only used for
    -- prioritisation.
    bodySize :: Word64
    bodySize = fromIntegral $ Lazy.length $ serialise body
