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
import Cardano.Crypto.ProtocolMagic
import Cardano.Prelude
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as Lazy
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Morpho.Ledger.Block
import Morpho.Ledger.Forge ()
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Storage.Common
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import Ouroboros.Network.Magic (NetworkMagic (..))

{-------------------------------------------------------------------------------
  RunNode instance for the Morpho ledger
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (MorphoBlock h c) where
  getSystemStart = const $ SystemStart dummyDate
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0
  getNetworkMagic = const $ NetworkMagic 0x0000ffff

  -- 'ProtocolMagicId' is removed from newer ouroboros-consensus
  getProtocolMagicId = const $ ProtocolMagicId 0x0000ffff

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
      . (* 10)
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
