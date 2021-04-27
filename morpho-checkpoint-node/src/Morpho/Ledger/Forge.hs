{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The CanFoge instance is Orphan.

module Morpho.Ledger.Forge
  ( morphoBlockForging,
    forgeMorpho,
  )
where

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Hash
import Cardano.Prelude
import Codec.Serialise (Serialise (encode), serialise)
import qualified Data.ByteString.Lazy as Lazy
import Morpho.Ledger.Block
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.Forging
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT

type instance ForgeStateInfo (MorphoBlock h c) = ()

morphoBlockForging ::
  ( Signable (BftDSIGN c) (MorphoStdHeader h c),
    BftCrypto c,
    HashAlgorithm h,
    Monad m
  ) =>
  CoreNodeId ->
  BlockForging m (MorphoBlock h c)
morphoBlockForging nodeId =
  BlockForging
    { forgeLabel = "morphoBlockForging",
      canBeLeader = nodeId,
      updateForgeState = \_ _ _ -> return $ ForgeStateUpdated (),
      checkCanForge = \_ _ _ _ _ -> return (),
      forgeBlock = \cfg blNo slotNo (MorphoTick (MorphoLedgerState st)) txs _ ->
        return $ forgeMorpho (configConsensus cfg) slotNo blNo (pointHash $ morphoTip st) txs
    }

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
  MorphoBlock h c
forgeMorpho ccfg curSlot curBlock prevHash txs =
  MorphoBlock
    { morphoHeader = mkMorphoHeader stdHeader bftFields bodySize,
      morphoBody = body
    }
  where
    bftFields :: BftFields c (MorphoStdHeader h c)
    bftFields = forgeBftFields ccfg stdHeader
    body :: MorphoBody
    body =
      MorphoBody
        { morphoTxs = unMorphoGenTx <$> txs
        }
    stdHeader :: MorphoStdHeader h c
    stdHeader =
      MorphoStdHeader
        { morphoPrev = prevHash,
          morphoSlotNo = curSlot,
          morphoBlockNo = curBlock,
          morphoBodyHash = hashWithSerialiser encode body
        }
    -- We use the size of the body, not of the whole block (= header + body),
    -- since the header size is fixed and this size is only used for
    -- prioritisation.
    -- TODO: Check this, see also the better implementation of this in tests/Test/Morpho/Generators.hs
    bodySize :: Word64
    bodySize = fromIntegral $ Lazy.length $ serialise body
