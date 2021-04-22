{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Tracing.Orphan.MorphoToJSONInstances where

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Hash.Class
import Cardano.Prelude hiding (show)
import Codec.CBOR.Write
import Data.Aeson (Options (tagSingleConstructors, unwrapUnaryRecords), ToJSON (..), ToJSONKey, Value (..), defaultOptions, genericToJSON)
import qualified Data.ByteString.Base16 as B16
import qualified Morpho.Config.Topology as MT
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.Serialise
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Morpho.Tracing.Orphan.ExternalToJSONInstances ()
import Morpho.Tracing.Types
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( TraceChainSyncClientEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent (..))
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server (TraceLocalTxSubmissionServerEvent (..))
import Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Storage.ChainDB hiding (InvalidBlock, getTipPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Types as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB.OnDisk
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import Ouroboros.Consensus.Storage.Serialisation
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Types as VolatileDB
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (Tip)
import Ouroboros.Network.BlockFetch.ClientState

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { tagSingleConstructors = True,
      unwrapUnaryRecords = True
    }

instance ToJSON MorphoInitTrace where
  toJSON = genericToJSON jsonOptions

instance ToJSON (TraceLocalTxSubmissionServerEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (LedgerEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (RealPoint (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TraceReplayEvent (MorphoBlock h c) (Point (MorphoBlock h c))) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (VolatileDB.ParseError (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (VolatileDB.TraceEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (ImmutableDB.Tip (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (ChainHash (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (ImmutableDB.ChunkFileError (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (ImmutableDB.TraceEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (LedgerDB.TraceEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (InitFailure (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (StreamFrom (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (StreamTo (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (UnknownRange (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (ChainDB.TraceIteratorEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TraceOpenEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (Tip (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (HeaderEnvelopeError (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TraceChainSyncClientEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TraceFetchClientState (Header (MorphoBlock h c))) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (GenTx (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (TraceEventMempool (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance BftCrypto c => ToJSON (MorphoBlock h c) where
  toJSON = genericToJSON jsonOptions

instance BftCrypto c => ToJSON (BftFields c (MorphoBlock h c)) where
  toJSON (BftFields x) = String $ decodeUtf8 $ B16.encode $ toStrictByteString $ encodeSignedDSIGN x

instance (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) => ToJSON (ExtValidationError (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c), HashAlgorithm h, BftCrypto c, ToJSON (Header (MorphoBlock h c))) => ToJSON (TraceValidationEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (MorphoStdHeader h c) where
  toJSON = genericToJSON jsonOptions

instance BftCrypto c => ToJSON (Header (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c), BftCrypto c, HashAlgorithm h) => ToJSON (TraceInitChainSelEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (TraceGCEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TraceCopyToImmutableDBEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (ChainDB.FollowerRollState (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TraceFollowerEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) => ToJSON (InvalidBlockReason (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (HeaderFields (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c), BftCrypto c, HashAlgorithm h) => ToJSON (TraceAddBlockEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c), BftCrypto c, HashAlgorithm h) => ToJSON (ChainDB.TraceEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (MorphoState (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TimeTravelError (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON MorphoBlockTx where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (WontPushCheckpoint (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h) => ToJSON (ExtractStateTrace h c) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (MorphoError (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON MorphoBody where
  toJSON = genericToJSON jsonOptions

instance (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) => ToJSON (TraceForgeEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance BftCrypto c => ToJSON (BftFields c (MorphoStdHeader h c)) where
  toJSON (BftFields x) = String $ decodeUtf8 $ B16.encode $ toStrictByteString $ encodeSignedDSIGN x

instance BftCrypto c => ToJSON (SerialisedHeader (MorphoBlock h c)) where
  toJSON (SerialisedHeaderFromDepPair (GenDepPair (NestedCtxt CtxtMorpho) b)) = toJSON b

instance (BftCrypto c, HashAlgorithm h) => ToJSON (TraceChainSyncServerEvent (MorphoBlock h c)) where
  toJSON = genericToJSON jsonOptions

instance (BftCrypto c, HashAlgorithm h, ToJSON a) => ToJSON (AF.ChainUpdate (MorphoBlock h c) a) where
  toJSON = genericToJSON jsonOptions

instance ToJSON PublicKey where
  toJSON = genericToJSON jsonOptions

instance ToJSONKey PublicKey

instance ToJSON MT.RemoteAddress where
  toJSON = genericToJSON jsonOptions
