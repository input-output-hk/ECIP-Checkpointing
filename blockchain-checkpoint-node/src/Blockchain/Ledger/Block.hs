{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Morpho block to go with the mock ledger
--
-- None of the definitions in this module depend on, or even refer to, any
-- specific consensus protocols.
module Blockchain.Ledger.Block (
    MorphoBlock
  , MorphoHeader
  , MorphoBlock'(..)
  , Header(..)
  , MorphoStdHeader(..)
  , MorphoBody(..)
  , MorphoHash
    -- * Working with 'MorphoBlock'
  , mkMorphoHeader
  , matchesMorphoHeader
    -- * 'UpdateLedger'
  , LedgerState(..)
  , LedgerConfig(..)
  , updateMorphoLedgerState
  , genesisMorphoLedgerState
    -- * 'ApplyTx' (mempool support)
  , GenTx(..)
  , GenTxId(..)
  , mkMorphoGenTx
    -- * Crypto
  , MorphoCrypto
  , MorphoStandardCrypto
  , MorphoMockCrypto
    -- * Serialisation
  , encodeMorphoHeader
  , decodeMorphoHeader
  , morphoBlockBinaryInfo
  , morphoBlockHashInfo
  ) where

import           Prelude

import           Codec.Serialise (Serialise (..), serialise)
import           Control.Monad.Except
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.Block
import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

import           Blockchain.Ledger.State
import           Blockchain.Ledger.Tx

{-------------------------------------------------------------------------------
  Definition of a block

  The primed versions allow to vary the @ext@ parameter independently of the
  previous block hash.
-------------------------------------------------------------------------------}

type MorphoBlock  c ext = MorphoBlock'  c ext ext
type MorphoHeader c ext = Header (MorphoBlock c ext)

data MorphoBlock' c ext ext' = MorphoBlock {
      morphoHeader :: Header (MorphoBlock' c ext ext')
    , morphoBody   :: MorphoBody
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise)

instance GetHeader (MorphoBlock' c ext ext') where
  data Header (MorphoBlock' c ext ext') = MorphoHeader {
        -- | The header hash
        --
        -- This is the hash of the header itself. This is a bit unpleasant,
        -- because it makes the hash look self-referential (when computing the
        -- hash we must ignore the 'morphoHeaderHash' field). However, the benefit
        -- is that we can give a 'HasHeader' instance that does not require
        -- a (static) 'Serialise' instance.
        morphoHeaderHash :: HeaderHash (MorphoBlock' c ext ext')

        -- | Fields required for the 'HasHeader' instance
      , morphoHeaderStd  :: MorphoStdHeader c ext

        -- | Header extension
        --
        -- This extension will be required when using 'MorphoBlock' for specific
        -- consensus protocols.
      , morphoHeaderExt  :: ext'
      }
    deriving (Generic, Show, Eq, NoUnexpectedThunks)

  getHeader = morphoHeader

data MorphoStdHeader c ext = MorphoStdHeader {
      morphoPrev      :: ChainHash (MorphoBlock c ext)
    , morphoSlotNo    :: SlotNo
    , morphoBlockNo   :: BlockNo
    , morphoBodyHash  :: Hash (MorphoHash c) MorphoBody
    , morphoBlockSize :: Word64
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise, NoUnexpectedThunks)

data MorphoBody = MorphoBody {
      morphoTxs :: [Tx]
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise)

{-------------------------------------------------------------------------------
  Working with 'MorphoBlock'
-------------------------------------------------------------------------------}

mkMorphoHeader :: MorphoCrypto c
               => (ext' -> CBOR.Encoding)
               -> MorphoStdHeader c ext
               -> ext'
               -> Header (MorphoBlock' c ext ext')
mkMorphoHeader encodeExt std ext =
    headerWithoutHash {
        morphoHeaderHash = hashWithSerialiser
                             (encodeMorphoHeader encodeExt)
                             headerWithoutHash
      }
  where
    headerWithoutHash = MorphoHeader {
        morphoHeaderHash = error "Serialise instances should ignore hash"
      , morphoHeaderStd  = std
      , morphoHeaderExt  = ext
      }

-- | Check whether the block matches the header
matchesMorphoHeader :: MorphoCrypto c
                    => Header (MorphoBlock' c ext ext')
                    -> MorphoBlock'  c ext ext''
                    -> Bool
matchesMorphoHeader MorphoHeader{..} MorphoBlock {..} =
    morphoBodyHash == hash morphoBody
  where
    MorphoStdHeader{..} = morphoHeaderStd

{-------------------------------------------------------------------------------
  HasHeader instance for MorphoHeader
-------------------------------------------------------------------------------}

instance (MorphoCrypto c, Typeable ext) => HasHeader (MorphoHeader c ext) where
  blockHash      =            morphoHeaderHash
  blockPrevHash  = castHash . morphoPrev    . morphoHeaderStd
  blockSlot      =            morphoSlotNo  . morphoHeaderStd
  blockNo        =            morphoBlockNo . morphoHeaderStd
  blockInvariant = const True

{-------------------------------------------------------------------------------
  HasHeader instance for MorphoBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (MorphoBlock' c ext ext') =
  Hash (MorphoHash c) (Header (MorphoBlock' c ext ext'))

instance (MorphoCrypto c, Typeable ext)
      => Measured BlockMeasure (MorphoBlock c ext) where
  measure = blockMeasure

instance (MorphoCrypto c, Typeable ext) => HasHeader (MorphoBlock c ext) where
  blockHash      =            blockHash     . morphoHeader
  blockPrevHash  = castHash . blockPrevHash . morphoHeader
  blockSlot      =            blockSlot     . morphoHeader
  blockNo        =            blockNo       . morphoHeader
  blockInvariant = const True

instance (MorphoCrypto c, Typeable ext) => StandardHash (MorphoBlock c ext)

{-------------------------------------------------------------------------------
  HasUTxO instance
-------------------------------------------------------------------------------

instance HasUtxo (MorphoBlock' c ext ext') where
  txIns      = txIns      . morphoBody
  txOuts     = txOuts     . morphoBody
  confirmed  = confirmed  . morphoBody
  updateUtxo = updateUtxo . morphoBody

instance HasUtxo MorphoBody where
  txIns      = txIns      . morphoTxs
  txOuts     = txOuts     . morphoTxs
  confirmed  = confirmed  . morphoTxs
  updateUtxo = updateUtxo . morphoTxs
-}

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

instance (MorphoCrypto c, Typeable ext, SupportedBlock (MorphoBlock c ext))
      => UpdateLedger (MorphoBlock c ext) where
  newtype LedgerState (MorphoBlock c ext) = MorphoLedgerState {
        morphoLedgerState :: MorphoState (MorphoBlock c ext)
      }
    deriving stock   (Generic, Show, Eq)
    deriving newtype (Serialise, NoUnexpectedThunks)

  data LedgerConfig (MorphoBlock c ext) = DummyMorphoLedgerConfig
    deriving (Show)

  type LedgerError (MorphoBlock c ext) = MorphoError (MorphoBlock c ext)

  applyChainTick _ _ = TickedLedgerState
  applyLedgerBlock _cfg = updateMorphoLedgerState
  reapplyLedgerBlock _cfg = (mustSucceed . runExcept) .: updateMorphoLedgerState
    where
      mustSucceed (Left  err) = error ("reapplyLedgerBlock: unexpected error: " <> show err)
      mustSucceed (Right st)  = st
  ledgerTipPoint (MorphoLedgerState st) = morphoTip st
  ledgerConfigView _ = DummyMorphoLedgerConfig

updateMorphoLedgerState :: (Monad m, MorphoCrypto c, Typeable ext)
                        => MorphoBlock c ext
                        -> LedgerState (MorphoBlock c ext)
                        -> ExceptT (MorphoError (MorphoBlock c ext))
                                   m
                                   (LedgerState (MorphoBlock c ext))
updateMorphoLedgerState b (MorphoLedgerState st) =
    MorphoLedgerState <$> updateMorphoState b st

-- updateMorphoUTxO :: (Monad m, HasUtxo a)
--                  => a
--                  -> TickedLedgerState (MorphoBlock c ext)
--                  -> ExceptT (MockError (MorphoBlock c ext))
--                             m
--                             (TickedLedgerState (MorphoBlock c ext))
-- updateMorphoUTxO b (TickedLedgerState (MorphoLedgerState st)) =
--     TickedLedgerState . MorphoLedgerState <$> updateMockUTxO b st

genesisMorphoLedgerState :: MorphoLedgerConfig -> LedgerState (MorphoBlock c ext)
genesisMorphoLedgerState = MorphoLedgerState . genesisMorphoState


instance HasTxs (MorphoBlock c ext) where
  getTxs b = morphoTxs $ morphoBody b

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

instance (MorphoCrypto c, Typeable ext, SupportedBlock (MorphoBlock c ext))
      => ApplyTx (MorphoBlock c ext) where
  data GenTx (MorphoBlock c ext) = MorphoGenTx
    { morphoGenTx   :: !Tx
    , morphoGenTxId :: !TxId
    } deriving stock    (Generic)
      deriving anyclass (Serialise)

  newtype GenTxId (MorphoBlock c ext) = MorphoGenTxId
    { unMorphoGenTxId :: TxId
    } deriving stock   (Generic)
      deriving newtype (Show, Eq, Ord, Serialise)

  txId = MorphoGenTxId . morphoGenTxId

  txSize _ = 2000  -- TODO #745

  type ApplyTxErr (MorphoBlock c ext) = MorphoError (MorphoBlock c ext)

  applyTx            = \_ -> applyTxMorpho
  reapplyTx          = \_ -> applyTxMorpho
  reapplyTxSameState = \_ -> (mustSucceed . runExcept) .: applyTxMorpho
    where
      mustSucceed (Left  _)  = error "reapplyTxSameState: unexpected error"
      mustSucceed (Right st) = st

-- Why is this needed if we're already updating the state in `updateMorphoState`???
applyTxMorpho :: (Monad m, blk ~ MorphoBlock c ext)
               => GenTx (MorphoBlock c ext)
               -> TickedLedgerState (MorphoBlock c ext)
               -> ExceptT (MorphoError blk)
                          m
                          (TickedLedgerState (MorphoBlock c ext))
applyTxMorpho tx (TickedLedgerState (MorphoLedgerState st)) = do
  (Tx v) <- pure $ morphoGenTx tx
  TickedLedgerState . MorphoLedgerState <$> updateMorphoStateByVote st v


instance (Typeable p, Typeable c) => NoUnexpectedThunks (GenTx (MorphoBlock p c)) where
  showTypeOf _ = show $ typeRep (Proxy @(GenTx (MorphoBlock p c)))

{-
instance HasUtxo (GenTx (MorphoBlock p c)) where
  txIns      = txIns      . morphoGenTx
  txOuts     = txOuts     . morphoGenTx
  confirmed  = confirmed  . morphoGenTx
  updateUtxo = updateUtxo . morphoGenTx
-}

instance Condense (GenTx (MorphoBlock p c)) where
    condense = condense . morphoGenTx

instance Show (GenTx (MorphoBlock p c)) where
    show = show . morphoGenTx

instance Condense (GenTxId (MorphoBlock p c)) where
    condense = condense . unMorphoGenTxId

mkMorphoGenTx :: Tx -> GenTx (MorphoBlock c ext)
mkMorphoGenTx tx = MorphoGenTx
    { morphoGenTx   = tx
    , morphoGenTxId = hash tx
    }

{-------------------------------------------------------------------------------
  Crypto needed for morpho blocks
-------------------------------------------------------------------------------}

class (HashAlgorithm (MorphoHash c), Typeable c) => MorphoCrypto c where
  type family MorphoHash c :: *

data MorphoStandardCrypto
data MorphoMockCrypto

instance BftCrypto MorphoMockCrypto where
  type BftDSIGN MorphoMockCrypto = MockDSIGN

instance MorphoCrypto MorphoStandardCrypto where
  type MorphoHash MorphoStandardCrypto = MD5

instance MorphoCrypto MorphoMockCrypto where
  type MorphoHash MorphoMockCrypto = ShortHash

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense ext' => Condense (Header (MorphoBlock' c ext ext')) where
  condense MorphoHeader{..} = mconcat [
        "("
      , condense morphoPrev
      , "->"
      , condense morphoHeaderHash
      , ","
      , condense morphoSlotNo
      , ","
      , condense morphoHeaderExt
      , ")"
      ]
    where
      MorphoStdHeader{..} = morphoHeaderStd

instance Condense ext' => Condense (MorphoBlock' c ext ext') where
  condense MorphoBlock{..} = mconcat [
        "("
      , condense morphoPrev
      , "->"
      , condense morphoHeaderHash
      , ","
      , condense morphoSlotNo
      , ","
      , condense morphoHeaderExt
      , ","
      , condense morphoTxs
      , ")"
      ]
    where
      MorphoHeader{..}    = morphoHeader
      MorphoStdHeader{..} = morphoHeaderStd
      MorphoBody{..}      = morphoBody

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ToCBOR MorphoBody where
  toCBOR = encode

encodeMorphoHeader :: (ext' -> CBOR.Encoding)
                   -> Header (MorphoBlock' c ext ext')
                   -> CBOR.Encoding
encodeMorphoHeader encodeExt MorphoHeader{..} =  mconcat [
      CBOR.encodeListLen 2
    , encode morphoHeaderStd
    , encodeExt morphoHeaderExt
    ]

decodeMorphoHeader :: MorphoCrypto c
                   => (ext' -> CBOR.Encoding)
                   -> (forall s. CBOR.Decoder s ext')
                   -> forall s. CBOR.Decoder s (Header (MorphoBlock' c ext ext'))
decodeMorphoHeader encodeExt decodeExt = do
    CBOR.decodeListLenOf 2
    mkMorphoHeader encodeExt <$> decode <*> decodeExt

-- | Custom 'Serialise' instance that doesn't serialise the hash
instance (MorphoCrypto c, Serialise ext')
      => Serialise (Header (MorphoBlock' c ext ext')) where
  encode = encodeMorphoHeader encode
  decode = decodeMorphoHeader encode decode

morphoBlockBinaryInfo :: (MorphoCrypto c, Serialise ext')
                      => MorphoBlock' c ext ext' -> BinaryInfo CBOR.Encoding
morphoBlockBinaryInfo b = BinaryInfo
    { binaryBlob   = encode b
    , headerOffset = 2 -- For the 'encodeListLen'
    , headerSize   = fromIntegral $ Lazy.length $ serialise (getHeader b)
    }

-- | As we can't simply create a 'Hash' from a 'ByteString', we're (ab)using
-- its 'FromCBOR'/'ToCBOR' instances. This means we're adding an extra byte
-- for the CBOR tag.
morphoBlockHashInfo
  :: forall c ext ext'. (MorphoCrypto c, Typeable ext, Typeable ext')
  => HashInfo (HeaderHash (MorphoBlock' c ext ext'))
morphoBlockHashInfo = HashInfo
    { hashSize
    , getHash  = do
        bl <- Get.getLazyByteString (fromIntegral hashSize)
        case CBOR.deserialiseFromBytes fromCBOR bl of
          Left e       -> fail (show e)
          Right (_, h) -> return h
    , putHash  = Put.putByteString . CBOR.toStrictByteString . toCBOR
    }
  where
    -- + 1 For the CBOR tag
    hashSize = 1 + fromIntegral (byteCount (Proxy @(MorphoHash c)))
