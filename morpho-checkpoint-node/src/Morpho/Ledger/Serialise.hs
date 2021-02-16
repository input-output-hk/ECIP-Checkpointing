{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Ledger.Serialise where

import Cardano.Prelude
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..))
import Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import Morpho.Ledger.Block
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.NestedContent
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Network.Block

instance (HashAlgorithm h, BftCrypto c) => EncodeDisk (MorphoBlock h c) (MorphoBlock h c) where
  encodeDisk _ = encodeMorphoBlock

instance
  (HashAlgorithm h, BftCrypto c, blk ~ MorphoBlock h c) =>
  DecodeDisk blk (Lazy.ByteString -> blk)
  where
  decodeDisk _ = const <$> decode -- decodeMorphoBlock

instance
  (HashAlgorithm h, BftCrypto c, blk ~ MorphoBlock h c) =>
  DecodeDisk blk (Lazy.ByteString -> Header blk)
  where
  decodeDisk _ = const <$> decode -- decodeMorphoBlock

instance (HashAlgorithm h, BftCrypto c) => DecodeDiskDep (NestedCtxt Header) (MorphoBlock h c) -- TODO

instance SameDepIndex (NestedCtxt_ (MorphoBlock h c) f)

instance HasNestedContent f (MorphoBlock h c)

instance (HashAlgorithm h, BftCrypto c) => ReconstructNestedCtxt Header (MorphoBlock h c) -- TODO

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (MorphoBlock h c) f a where
  CtxtMorpho :: NestedCtxt_ (MorphoBlock h c) f (f (MorphoBlock h c))

deriving instance Show (NestedCtxt_ (MorphoBlock h c) f a)

instance TrivialDependency (NestedCtxt_ (MorphoBlock h c) f) where
  type TrivialIndex (NestedCtxt_ (MorphoBlock h c) f) = f (MorphoBlock h c)
  hasSingleIndex CtxtMorpho CtxtMorpho = Refl
  indexIsTrivial = CtxtMorpho

instance (blk ~ MorphoBlock h c) => EncodeDisk blk (LedgerState blk)

instance (blk ~ MorphoBlock h c) => DecodeDisk blk (LedgerState blk)

instance (blk ~ MorphoBlock h c) => EncodeDisk blk (AnnTip blk) where
  encodeDisk _ = defaultEncodeAnnTip encode

instance (blk ~ MorphoBlock h c) => DecodeDisk blk (AnnTip blk) where
  decodeDisk _ = defaultDecodeAnnTip decode

-- Needed for ChainDepState
instance (blk ~ MorphoBlock h c) => EncodeDisk blk ()

instance (blk ~ MorphoBlock h c) => DecodeDisk blk ()

instance (HashAlgorithm h, BftCrypto c, blk ~ MorphoBlock h c) => EncodeDisk blk (Header blk)

instance (HashAlgorithm h, BftCrypto c) => EncodeDiskDep (NestedCtxt Header) (MorphoBlock h c)

instance (HashAlgorithm h, BftCrypto c) => HasBinaryBlockInfo (MorphoBlock h c) where
  getBinaryBlockInfo = morphoBlockBinaryInfo

instance (HashAlgorithm h, BftCrypto c) => SerialiseDiskConstraints (MorphoBlock h c)

encodeMorphoBlock :: (HashAlgorithm h, BftCrypto c) => MorphoBlock h c -> CBOR.Encoding
encodeMorphoBlock = encode

instance (HashAlgorithm h, BftCrypto c) => SerialiseNodeToClientConstraints (MorphoBlock h c)

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToNode blk blk
  where
  encodeNodeToNode _ _ = defaultEncodeCBORinCBOR
  decodeNodeToNode _ _ = defaultDecodeCBORinCBOR

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToNode blk (Header blk)
  where
  encodeNodeToNode _ _ = defaultEncodeCBORinCBOR
  decodeNodeToNode _ _ = defaultDecodeCBORinCBOR

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToNode blk (Serialised blk)

instance
  (blk ~ MorphoBlock h c, BftCrypto c) =>
  SerialiseNodeToNode blk (SerialisedHeader blk)
  where
  encodeNodeToNode _ _ = encodeTrivialSerialisedHeader
  decodeNodeToNode _ _ = decodeTrivialSerialisedHeader

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToNode blk (GenTx blk)

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToNode blk (GenTxId blk)

instance (HashAlgorithm h, BftCrypto c) => SerialiseNodeToNodeConstraints (MorphoBlock h c) where
  estimateBlockSize = fromIntegral . morphoBlockSize . morphoHeaderStd

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToClient blk blk
  where
  encodeNodeToClient _ _ = defaultEncodeCBORinCBOR
  decodeNodeToClient _ _ = defaultDecodeCBORinCBOR

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToClient blk (Header blk)
  where
  encodeNodeToClient _ _ = defaultEncodeCBORinCBOR
  decodeNodeToClient _ _ = defaultDecodeCBORinCBOR

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToClient blk (Serialised blk)

instance
  (blk ~ MorphoBlock h c, BftCrypto c) =>
  SerialiseNodeToClient blk (SerialisedHeader blk)
  where
  encodeNodeToClient _ _ = encodeTrivialSerialisedHeader
  decodeNodeToClient _ _ = decodeTrivialSerialisedHeader

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToClient blk (GenTx blk)

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c) =>
  SerialiseNodeToClient blk (GenTxId blk)

instance
  (blk ~ MorphoBlock h c, HashAlgorithm h, BftCrypto c, Serialise (HeaderHash blk)) =>
  SerialiseNodeToClient blk (MorphoError blk)

instance
  (blk ~ MorphoBlock h c, BftCrypto c) =>
  SerialiseNodeToClient blk (SomeSecond Query blk)
  where
  encodeNodeToClient _ _ query = case query of
  decodeNodeToClient _ _ = fail "Morpho doesn't support node-to-client queries"

instance
  (blk ~ MorphoBlock h c, BftCrypto c) =>
  SerialiseResult blk (Query blk)
  where
  encodeResult _ _ query = case query of
  decodeResult _ _ query = case query of
