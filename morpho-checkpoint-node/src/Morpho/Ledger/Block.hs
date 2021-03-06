{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Morpho.Ledger.Block
  ( MorphoBlock (..),
    Header (..),
    MorphoMockHash,
    MorphoStdHeader (..),
    MorphoBody (..),
    MorphoBlockTx (..),

    -- * Configurations
    BlockConfig (..),
    CodecConfig (..),
    StorageConfig (..),
    ConsensusConfig (..),

    -- * Working with 'MorphoBlock'
    mkMorphoHeader,
    matchesMorphoHeader,

    -- * Crypto
    HashAlgorithm,
    ConsensusMockCrypto,

    -- * Serialisation
    encodeMorphoHeader,
    decodeMorphoHeader,
    morphoBlockBinaryInfo,
    MorphoNodeToNodeVersion (..),
    MorphoNodeToClientVersion (..),
  )
where

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import Cardano.Crypto.Hash
import Cardano.Crypto.Util
import Cardano.Prelude
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Write
import Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import Morpho.Ledger.Tx
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Storage.Common
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Magic

{-------------------------------------------------------------------------------
  Definition of a block

-------------------------------------------------------------------------------}

-- | Main datatype describing the morpho block.
--
--   The 'h' type parameter being the hashing algorithm used to sign the
--   Morpho Header.
--
--   The 'c' type parameter being the crypto algorithm used for the
--   BFT layer
data MorphoBlock h c = MorphoBlock
  { morphoHeader :: Header (MorphoBlock h c),
    morphoBody :: !MorphoBody
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise)

data instance Header (MorphoBlock h c) = MorphoHeader
  { -- | The header hash
    --
    -- This is the hash of the header itself. This is a bit unpleasant,
    -- because it makes the hash look self-referential (when computing the
    -- hash we must ignore the 'morphoHeaderHash' field). However, the benefit
    -- is that we can give a 'HasHeader' instance that does not require
    -- a (static) 'Serialise' instance.
    morphoHeaderHash :: HeaderHash (MorphoBlock h c),
    -- | Fields required for the 'HasHeader' instance
    morphoHeaderStd :: MorphoStdHeader h c,
    morphoBlockSize :: Word64,
    -- | Bft fields
    --
    -- These fields are required by the underlying BFT consensus
    -- algorithm.
    --
    -- Contains all the BFT-related parameters.
    morphoBftFields :: BftFields c (MorphoStdHeader h c)
  }
  deriving (Generic, Show, Eq, NoThunks)

instance
  (HashAlgorithm h, BftCrypto c) =>
  GetHeader (MorphoBlock h c)
  where
  getHeader = morphoHeader

  blockMatchesHeader = matchesMorphoHeader
  headerIsEBB = const Nothing

data MorphoStdHeader h c = MorphoStdHeader
  { morphoPrev :: ChainHash (MorphoBlock h c),
    morphoSlotNo :: SlotNo,
    morphoBlockNo :: BlockNo,
    morphoBodyHash :: Hash h MorphoBody
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, NoThunks)

data MorphoBlockTx = MorphoBlockTx
  { morphoBlockGenTx :: !Tx,
    morphoBlockGenTxId :: !MorphoTxId
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise, NoThunks)

newtype MorphoBody = MorphoBody
  { morphoTxs :: [MorphoBlockTx]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise)

instance SignableRepresentation (MorphoStdHeader h c) where
  getSignableRepresentation = toStrictByteString . encode

instance Condense MorphoBlockTx where
  condense = condense . morphoBlockGenTx

{-------------------------------------------------------------------------------
  Configuration Instances
-------------------------------------------------------------------------------}

data instance BlockConfig (MorphoBlock h c) = MorphoBlockConfig
  { systemStart :: SystemStart,
    networkMagic :: NetworkMagic
  }
  deriving (Generic, NoThunks)

newtype instance CodecConfig (MorphoBlock h c) = MorphoCodecConfig ()
  deriving (Generic, NoThunks)

newtype instance StorageConfig (MorphoBlock h c) = MorphoStorageConfig SecurityParam
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Working with 'MorphoBlock'
-------------------------------------------------------------------------------}

mkMorphoHeader ::
  (HashAlgorithm h, BftCrypto c) =>
  MorphoStdHeader h c ->
  BftFields c (MorphoStdHeader h c) ->
  Word64 ->
  Header (MorphoBlock h c)
mkMorphoHeader std bftf size =
  headerWithoutHash
    { morphoHeaderHash =
        hashWithSerialiser
          encodeMorphoHeader
          headerWithoutHash
    }
  where
    headerWithoutHash =
      MorphoHeader
        { morphoHeaderHash = panic "Serialise instances should ignore the header hash",
          morphoHeaderStd = std,
          morphoBftFields = bftf,
          morphoBlockSize = size
        }

-- | Check whether the block matches the header
matchesMorphoHeader ::
  HashAlgorithm h =>
  Header (MorphoBlock h c) ->
  MorphoBlock h c ->
  Bool
matchesMorphoHeader MorphoHeader {..} MorphoBlock {..} =
  morphoBodyHash == hashWithSerialiser encode morphoBody
  where
    MorphoStdHeader {..} = morphoHeaderStd

{-------------------------------------------------------------------------------
  HasHeader instance for MorphoBlock
-------------------------------------------------------------------------------}

type instance
  HeaderHash (MorphoBlock h c) =
    Hash h (Header (MorphoBlock h c))

instance
  (HashAlgorithm h, BftCrypto c) =>
  StandardHash (MorphoBlock h c)

instance
  (HashAlgorithm h, BftCrypto c) =>
  HasHeader (Header (MorphoBlock h c))
  where
  getHeaderFields hdr =
    HeaderFields
      { headerFieldHash = morphoHeaderHash hdr,
        headerFieldSlot = morphoSlotNo $ morphoHeaderStd hdr,
        headerFieldBlockNo = morphoBlockNo $ morphoHeaderStd hdr
      }

instance
  (HashAlgorithm h, BftCrypto c) =>
  HasHeader (MorphoBlock h c)
  where
  getHeaderFields = getBlockHeaderFields

instance HashAlgorithm h => ConvertRawHash (MorphoBlock h c) where
  toRawHash _ = hashToBytes
  fromShortRawHash _ = UnsafeHash
  hashSize _ = fromIntegral $ sizeHash (Proxy @h)

{-------------------------------------------------------------------------------
  Prev Hash instance
-------------------------------------------------------------------------------}

instance (HashAlgorithm h, BftCrypto c) => GetPrevHash (MorphoBlock h c) where
  headerPrevHash = morphoPrev . morphoHeaderStd

{-------------------------------------------------------------------------------
  Crypto and Hash needed for morpho blocks
-------------------------------------------------------------------------------}

data ConsensusMockCrypto
  deriving (Generic, NoThunks)

type MorphoMockHash = ShortHash

instance BftCrypto ConsensusMockCrypto where
  type BftDSIGN ConsensusMockCrypto = MockDSIGN

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense (Header (MorphoBlock h c)) where
  condense MorphoHeader {..} =
    mconcat
      [ "(",
        condense morphoPrev,
        "->",
        condense morphoHeaderHash,
        ",",
        condense morphoSlotNo,
        ")"
      ]
    where
      MorphoStdHeader {..} = morphoHeaderStd

instance Condense (MorphoBlock h c) where
  condense MorphoBlock {..} =
    mconcat
      [ "(",
        condense morphoPrev,
        "->",
        condense morphoHeaderHash,
        ",",
        condense morphoSlotNo,
        ",",
        condense morphoTxs,
        ")"
      ]
    where
      MorphoHeader {..} = morphoHeader
      MorphoStdHeader {..} = morphoHeaderStd
      MorphoBody {..} = morphoBody

{-------------------------------------------------------------------------------
  Envelope Validation
-------------------------------------------------------------------------------}

instance (HashAlgorithm h, BftCrypto c) => HasAnnTip (MorphoBlock h c)

instance (HashAlgorithm h, BftCrypto c) => BasicEnvelopeValidation (MorphoBlock h c)

instance (HashAlgorithm h, BftCrypto c) => ValidateEnvelope (MorphoBlock h c)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeMorphoHeader ::
  (HashAlgorithm h, BftCrypto c) =>
  Header (MorphoBlock h c) ->
  CBOR.Encoding
encodeMorphoHeader MorphoHeader {..} =
  mconcat
    [ CBOR.encodeListLen 3,
      encode morphoHeaderStd,
      encode morphoBftFields,
      encode morphoBlockSize
    ]

decodeMorphoHeader ::
  (HashAlgorithm h, BftCrypto c) =>
  forall s. CBOR.Decoder s (Header (MorphoBlock h c))
decodeMorphoHeader = do
  CBOR.decodeListLenOf 3
  mkMorphoHeader <$> decode <*> decode <*> decode

instance (HashAlgorithm h, BftCrypto c) => Serialise (BftFields c (MorphoStdHeader h c)) where
  encode BftFields {..} =
    mconcat
      [ encodeSignedDSIGN bftSignature
      ]
  decode = BftFields <$> decodeSignedDSIGN

-- | Custom 'Serialise' instance that doesn't serialise the hash
instance
  (HashAlgorithm h, BftCrypto c) =>
  Serialise (Header (MorphoBlock h c))
  where
  encode = encodeMorphoHeader
  decode = decodeMorphoHeader

morphoBlockBinaryInfo ::
  (HashAlgorithm h, BftCrypto c) =>
  MorphoBlock h c ->
  BinaryBlockInfo
morphoBlockBinaryInfo b =
  BinaryBlockInfo
    { headerOffset = 2, -- For the 'encodeListLen'
      headerSize = fromIntegral $ Lazy.length $ serialise (getHeader b)
    }

{-------------------------------------------------------------------------------
  Consensus
-------------------------------------------------------------------------------}

-- | Associating the MorphoBlock with the ouroboros BFT consensus algorithm.
type instance BlockProtocol (MorphoBlock h c) = Bft c

{-------------------------------------------------------------------------------
  ProtocolVersion
-------------------------------------------------------------------------------}

data MorphoNodeToNodeVersion = MorphoNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data MorphoNodeToClientVersion = MorphoNodeToClientVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion (MorphoBlock h c) where
  type BlockNodeToNodeVersion (MorphoBlock h c) = MorphoNodeToNodeVersion
  type BlockNodeToClientVersion (MorphoBlock h c) = MorphoNodeToClientVersion

instance SupportedNetworkProtocolVersion (MorphoBlock h c) where
  supportedNodeToNodeVersions _ = Map.singleton NodeToNodeV_6 MorphoNodeToNodeVersion1
  supportedNodeToClientVersions _ = Map.singleton NodeToClientV_8 MorphoNodeToClientVersion1
  latestReleasedNodeVersion _ = (Just NodeToNodeV_6, Just NodeToClientV_8)
