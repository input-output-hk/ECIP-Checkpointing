{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Node.RunNode
  (
  )
where

import Cardano.Crypto.DSIGN.Class
import Cardano.Prelude
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as Lazy
import Morpho.Ledger.Block
import Morpho.Ledger.Forge ()
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Storage.Common
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks

{-------------------------------------------------------------------------------
  RunNode instance for the Morpho ledger
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (MorphoBlock h c) where
  getSystemStart = systemStart
  getNetworkMagic = networkMagic
  getProtocolMagicId = protocolMagicId

instance SignedHeader (Header (MorphoBlock h c)) where
  headerSigned = morphoHeaderStd

type instance Signed (Header (MorphoBlock h c)) = MorphoStdHeader h c

instance
  forall h c.
  ( HashAlgorithm h,
    BftCrypto c,
    Signable (BftDSIGN c) (MorphoStdHeader h c),
    MorphoStateDefaultConstraints h c
  ) =>
  BlockSupportsProtocol (MorphoBlock h c)
  where
  validateView _ = bftValidateView morphoBftFields

instance
  (HashAlgorithm h, Signable (BftDSIGN c) (MorphoStdHeader h c), MorphoStateDefaultConstraints h c) =>
  LedgerSupportsProtocol (MorphoBlock h c)
  where
  protocolLedgerView _ _ = ()
  ledgerViewForecastAt _ _ = Just . trivialForecast

instance
  ( MorphoStateDefaultConstraints h c,
    NoUnexpectedThunks c,
    HashAlgorithm h,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  RunNode (MorphoBlock h c)
  where
  nodeBlockFetchSize = fromIntegral . morphoBlockSize . morphoHeaderStd
  nodeImmDbChunkInfo =
    simpleChunkInfo
      . EpochSize
      . (* 1000) -- TODO: keep as big as possible without creating too big chunk files.
      . maxRollbacks
      . bftSecurityParam
      . bftParams
      . configConsensus
  nodeCheckIntegrity _ _ = True
  nodeGetBinaryBlockInfo blk =
    BinaryBlockInfo
      { -- Drop the 'encodeListLen' that precedes the header and the body
        headerOffset = 1, --
          -- The decoders should use annotations, because this is expensive
        headerSize = fromIntegral $ Lazy.length (serialise (getHeader blk))
      }
