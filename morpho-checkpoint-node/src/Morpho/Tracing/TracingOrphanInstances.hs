{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Tracing.TracingOrphanInstances where

import Cardano.BM.Data.Tracer
  ( HasTextFormatter (..),
    emptyObject,
    mkObject,
    trStructured,
    trStructuredText,
  )
import Cardano.BM.Tracing
  ( HasPrivacyAnnotation (..),
    HasSeverityAnnotation (..),
    Severity (..),
    ToObject (..),
    TracingVerbosity (..),
    Transformable (..),
  )
import Cardano.Prelude hiding (show)
import Data.Aeson (ToJSON (..), Value (..), (.=))
import Data.Text (pack)
import qualified Data.Text as Text
-- We do need some consensus imports to provide useful trace messages for some
-- network protocols

import Morpho.Ledger.Block
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Morpho.Tracing.Types
import Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket (SockAddr)
import Ouroboros.Consensus.Block
  ( BlockProtocol,
    ForgeState (..),
    Header,
    RealPoint,
    getHeader,
    headerPoint,
    realPointHash,
    realPointSlot,
  )
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId, HasTxId, HasTxs (..), TxId, txId)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol,
  )
import Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
  ( TraceBlockFetchServerEvent,
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( TraceChainSyncClientEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( TraceChainSyncServerEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
  ( TraceLocalTxSubmissionServerEvent (..),
  )
import Ouroboros.Consensus.Node.Run (RunNode (..))
import Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import Ouroboros.Consensus.Util.Condense (Condense, condense)
import Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.Block
  ( BlockNo (..),
    ChainUpdate (..),
    HeaderHash,
    SlotNo (..),
    StandardHash,
    blockHash,
    pointSlot,
  )
import Ouroboros.Network.BlockFetch.ClientState
  ( TraceFetchClientState (..),
    TraceLabelPeer (..),
  )
import Ouroboros.Network.BlockFetch.Decision
  ( FetchDecision,
    FetchDecline (..),
  )
import Ouroboros.Network.Codec (AnyMessage (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
  ( ErrorPolicyTrace (..),
    TraceSendRecv (..),
    WithAddr (..),
  )
import qualified Ouroboros.Network.NodeToNode as NtN
import Ouroboros.Network.Point (withOrigin)
import Ouroboros.Network.Protocol.BlockFetch.Type
  ( BlockFetch,
    Message (..),
  )
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import Ouroboros.Network.Protocol.TxSubmission.Type
  ( Message (..),
    TxSubmission,
  )
import Ouroboros.Network.Snocket (LocalAddress (..))
import Ouroboros.Network.Subscription
  ( ConnectResult (..),
    DnsTrace (..),
    SubscriberError (..),
    SubscriptionTrace (..),
    WithDomainName (..),
    WithIPList (..),
  )
import Ouroboros.Network.TxSubmission.Inbound
  ( TraceTxSubmissionInbound (..),
  )
import Ouroboros.Network.TxSubmission.Outbound
  ( TraceTxSubmissionOutbound (..),
  )
import Prelude (String, id, show)

instance (HashAlgorithm h, BftCrypto c) => Transformable Text IO (ExtractStateTrace h c) where
  trTransformer = trStructuredText

instance (HashAlgorithm h, BftCrypto c) => ToObject (ExtractStateTrace h c) where
  toObject _verb (MorphoStateTrace (st@(MorphoState _ _ _ tip))) =
    mkObject
      [ "kind" .= String "MorphoStateUpdate",
        "state" .= show st,
        "tip" .= showPoint NormalVerbosity tip
      ]
  toObject _verb (PushingCheckpoint chkp) =
    mkObject
      [ "kind" .= String "ExtractTxErrorTrace",
        "checkpoint" .= show chkp
      ]
  toObject _verb (ExtractTxErrorTrace err) =
    mkObject
      [ "kind" .= String "ExtractTxErrorTrace",
        "err" .= show err
      ]
  toObject _verb (WontPushCheckpointTrace reason) =
    mkObject
      [ "kind" .= String "ExtractTxErrorTrace",
        "reason" .= show reason
      ]

instance (HashAlgorithm h, BftCrypto c) => HasTextFormatter (ExtractStateTrace h c) where
  formatText (MorphoStateTrace st) _ = pack $ "Current Ledger State: " ++ show st
  formatText (PushingCheckpoint chkp) _ = pack $ "Pushing Checkpoint " ++ show chkp
  formatText (ExtractTxErrorTrace err) _ =
    pack $
      "Error while trying to extract Tx from PoW BlockRef: " ++ show err
  formatText (WontPushCheckpointTrace reason) _ =
    pack $
      "Checkpoint doesn't need to be pushed: " ++ show reason

instance HasPrivacyAnnotation (ExtractStateTrace h c)

instance HasSeverityAnnotation (ExtractStateTrace h c) where
  getSeverityAnnotation MorphoStateTrace {} = Info
  getSeverityAnnotation PushingCheckpoint {} = Debug
  getSeverityAnnotation ExtractTxErrorTrace {} = Error
  getSeverityAnnotation WontPushCheckpointTrace {} = Info

instance ToObject (Header (MorphoBlock h c)) where
  toObject _ _ = emptyObject

instance Transformable Text IO PoWNodeRpcTrace where
  trTransformer = trStructuredText

instance HasTextFormatter PoWNodeRpcTrace where
  formatText tr _ = pack $ show tr

instance ToObject PoWNodeRpcTrace where
  toObject _verb (RpcPushedCheckpoint ckpt) =
    mkObject
      [ "kind" .= String "RpcPushedCheckpoint",
        "checkpoint" .= show ckpt
      ]
  toObject _verb (RpcLatestPoWBlock blk) =
    mkObject
      [ "kind" .= String "RpcLatestPoWBlock",
        "block" .= show blk
      ]
  toObject _verb (RpcNetworkError op err) =
    mkObject
      [ "kind" .= String "RpcNetworkError",
        "operation" .= show op,
        "error" .= err
      ]
  toObject _verb (RpcResponseParseError op err) =
    mkObject
      [ "kind" .= String "RpcResponseParseError",
        "operation" .= show op,
        "error" .= show err
      ]

instance HasPrivacyAnnotation PoWNodeRpcTrace

instance HasSeverityAnnotation PoWNodeRpcTrace where
  getSeverityAnnotation RpcPushedCheckpoint {} = Notice
  getSeverityAnnotation RpcLatestPoWBlock {} = Notice
  getSeverityAnnotation RpcNetworkError {} = Error
  getSeverityAnnotation RpcResponseParseError {} = Error

instance (HashAlgorithm h, BftCrypto c) => Transformable Text IO (TimeTravelError (MorphoBlock h c)) where
  trTransformer = trStructured

instance HasPrivacyAnnotation (TimeTravelError blk)

instance HasSeverityAnnotation (TimeTravelError blk) where
  getSeverityAnnotation _ = Error

instance (HashAlgorithm h, BftCrypto c) => ToObject (TimeTravelError (MorphoBlock h c)) where
  toObject verb (LedgerStateNotFoundAt point) =
    mkObject
      [ "kind" .= String "TimeTravelError",
        "error" .= String "LedgerStateNotFoundAt",
        "point" .= showPoint verb point
      ]
  toObject _verb (ChainNotLongEnough offset len) =
    mkObject
      [ "kind" .= String "TimeTravelError",
        "error" .= String "ChainNotLongEnough",
        "offset" .= show offset,
        "length" .= show len
      ]

instance (BftCrypto c, HashAlgorithm h) => ToObject (GenTx (MorphoBlock h c)) where
  toObject _verb (MorphoGenTx tx txid) =
    mkObject
      [ "kind" .= String "MorphoGenTx",
        "tx" .= tx,
        "txid" .= txid
      ]

instance StandardHash blk => ToObject (MorphoError blk) where
  toObject _verb (MorphoWrongDistance v) =
    mkObject
      [ "kind" .= String "MorphoWrongDistance",
        "vote" .= show v
      ]
  toObject _verb (MorphoInvalidSignature v) =
    mkObject
      [ "kind" .= String "MorphoInvalidSignature",
        "vote" .= show v
      ]
  toObject _verb (MorphoDuplicateVote v) =
    mkObject
      [ "kind" .= String "MorphoDuplicateVote",
        "vote" .= show v
      ]
  toObject _verb (MorphoUnknownPublicKey v) =
    mkObject
      [ "kind" .= String "MorphoUnknownPublicKey",
        "vote" .= show v
      ]
  toObject _verb (MorphoInvalidHash h h') =
    mkObject
      [ "kind" .= String "MorphoInvalidHash",
        "expectedHash" .= show h,
        "actualHash" .= show h'
      ]

--

-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@

--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation (ChainDB.TraceEvent blk)

instance HasSeverityAnnotation (ChainDB.TraceEvent blk) where
  getSeverityAnnotation (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK {} -> Info
    ChainDB.IgnoreBlockAlreadyInVolDB {} -> Info
    ChainDB.IgnoreInvalidBlock {} -> Info
    ChainDB.AddedBlockToQueue {} -> Debug
    ChainDB.BlockInTheFuture {} -> Info
    ChainDB.AddedBlockToVolDB {} -> Debug
    ChainDB.TryAddToCurrentChain {} -> Debug
    ChainDB.TrySwitchToAFork {} -> Info
    ChainDB.StoreButDontChange {} -> Debug
    ChainDB.AddedToCurrentChain {} -> Notice
    ChainDB.SwitchedToAFork {} -> Notice
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock {} -> Error
      ChainDB.InvalidCandidate {} -> Error
      ChainDB.ValidCandidate {} -> Info
      ChainDB.CandidateContainsFutureBlocks {} -> Debug
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {} -> Error
    ChainDB.ChainSelectionForFutureBlock {} -> Debug
  getSeverityAnnotation (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis {} -> Info
    LedgerDB.ReplayFromSnapshot {} -> Info
    LedgerDB.ReplayedBlock {} -> Info
  getSeverityAnnotation (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot {} -> Info
    LedgerDB.DeletedSnapshot {} -> Debug
    LedgerDB.InvalidSnapshot {} -> Error
  getSeverityAnnotation (ChainDB.TraceCopyToImmDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmDB {} -> Debug
    ChainDB.NoBlocksToCopyToImmDB -> Debug
  getSeverityAnnotation (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC {} -> Debug
    ChainDB.ScheduledGC {} -> Debug
  getSeverityAnnotation (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB {} -> Info
    ChainDB.ClosedDB {} -> Info
    ChainDB.OpenedImmDB {} -> Info
    ChainDB.OpenedVolDB -> Info
    ChainDB.OpenedLgrDB -> Info
  getSeverityAnnotation (ChainDB.TraceReaderEvent ev) = case ev of
    ChainDB.NewReader {} -> Debug
    ChainDB.ReaderNoLongerInMem {} -> Debug
    ChainDB.ReaderSwitchToMem {} -> Debug
    ChainDB.ReaderNewImmIterator {} -> Debug
  getSeverityAnnotation (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation {} -> Debug
  getSeverityAnnotation (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolDB {} -> Debug
    _ -> Debug
  getSeverityAnnotation (ChainDB.TraceImmDBEvent _ev) = Debug
  getSeverityAnnotation (ChainDB.TraceVolDBEvent _ev) = Debug

instance HasPrivacyAnnotation (TraceBlockFetchServerEvent blk)

instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceChainSyncClientEvent blk)

instance HasSeverityAnnotation (TraceChainSyncClientEvent blk) where
  getSeverityAnnotation (TraceDownloadedHeader _) = Info
  getSeverityAnnotation (TraceFoundIntersection _ _ _) = Info
  getSeverityAnnotation (TraceRolledBack _) = Notice
  getSeverityAnnotation (TraceException _) = Warning

instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk)

instance HasSeverityAnnotation (TraceChainSyncServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceEventMempool blk)

instance HasSeverityAnnotation (TraceEventMempool blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (ForgeState c)

instance HasSeverityAnnotation (ForgeState c) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation ()

instance HasSeverityAnnotation () where
  getSeverityAnnotation () = Info

instance HasPrivacyAnnotation (TraceForgeEvent blk)

instance HasSeverityAnnotation (TraceForgeEvent blk) where
  getSeverityAnnotation TraceForgedBlock {} = Info
  getSeverityAnnotation TraceStartLeadershipCheck {} = Info
  getSeverityAnnotation TraceNodeNotLeader {} = Info
  getSeverityAnnotation TraceNodeCannotLead {} = Error
  getSeverityAnnotation TraceNodeIsLeader {} = Info
  getSeverityAnnotation TraceNoLedgerState {} = Error
  getSeverityAnnotation TraceNoLedgerView {} = Error
  getSeverityAnnotation TraceBlockFromFuture {} = Error
  getSeverityAnnotation TraceSlotIsImmutable {} = Error
  getSeverityAnnotation TraceAdoptedBlock {} = Info
  getSeverityAnnotation TraceDidntAdoptBlock {} = Error
  getSeverityAnnotation TraceForgedInvalidBlock {} = Error

instance HasPrivacyAnnotation (TraceLocalTxSubmissionServerEvent blk)

instance HasSeverityAnnotation (TraceLocalTxSubmissionServerEvent blk) where
  getSeverityAnnotation _ = Info

--

-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.
instance
  ( HasPrivacyAnnotation (ChainDB.TraceAddBlockEvent blk),
    HasSeverityAnnotation (ChainDB.TraceAddBlockEvent blk),
    LedgerSupportsProtocol blk,
    ToObject (ChainDB.TraceAddBlockEvent blk)
  ) =>
  Transformable Text IO (ChainDB.TraceAddBlockEvent blk)
  where
  trTransformer = trStructuredText

instance
  (LedgerSupportsProtocol blk) =>
  HasTextFormatter (ChainDB.TraceAddBlockEvent blk)
  where
  formatText _ = pack . show . toList

instance Transformable Text IO (TraceBlockFetchServerEvent blk) where
  trTransformer = trStructuredText

instance HasTextFormatter (TraceBlockFetchServerEvent blk) where
  formatText _ = pack . show . toList

instance
  (Condense (HeaderHash blk), LedgerSupportsProtocol blk) =>
  Transformable Text IO (TraceChainSyncClientEvent blk)
  where
  trTransformer = trStructured

instance
  Condense (HeaderHash blk) =>
  Transformable Text IO (TraceChainSyncServerEvent blk)
  where
  trTransformer = trStructured

instance
  ( Show (GenTx blk),
    Show (GenTxId blk),
    ToObject (ApplyTxErr blk),
    Show (ApplyTxErr blk),
    ToObject (GenTx blk),
    ToJSON (GenTxId blk)
  ) =>
  Transformable Text IO (TraceEventMempool blk)
  where
  trTransformer = trStructuredText

condenseT :: Condense a => a -> Text
condenseT = pack . condense

showT :: Show a => a -> Text
showT = pack . show

instance
  ( tx ~ GenTx blk,
    Condense (HeaderHash blk),
    HasTxId tx,
    RunNode blk,
    Show blk,
    Show (TxId tx),
    ToObject (LedgerError blk),
    ToObject (OtherHeaderEnvelopeError blk),
    ToObject (ValidationErr (BlockProtocol blk)),
    ToObject (CannotLead (BlockProtocol blk))
  ) =>
  Transformable Text IO (TraceForgeEvent blk)
  where
  trTransformer = trStructuredText

instance HasTextFormatter (ForgeState blk) where
  formatText _ = pack . show . toList

instance
  ToObject (ChainIndepState (BlockProtocol blk)) =>
  Transformable Text IO (ForgeState blk)
  where
  trTransformer = trStructuredText

instance
  ( tx ~ GenTx blk,
    Condense (HeaderHash blk),
    HasTxId tx,
    LedgerSupportsProtocol blk,
    Show (TxId tx),
    Show blk
  ) =>
  HasTextFormatter (TraceForgeEvent blk)
  where
  formatText = \case
    TraceAdoptedBlock slotNo blk txs ->
      const $
        "Adopted forged block for slot " <> showT (unSlotNo slotNo)
          <> ": "
          <> condenseT (blockHash blk)
          <> "; TxIds: "
          <> showT (map txId txs)
    TraceBlockFromFuture currentSlot tip ->
      const $
        "Forged block from future: current slot " <> showT (unSlotNo currentSlot)
          <> ", tip being "
          <> condenseT tip
    TraceSlotIsImmutable slotNo tipPoint tipBlkNo ->
      const $
        "Forged for immutable slot " <> showT (unSlotNo slotNo)
          <> ", tip: "
          <> pack (showPoint MaximalVerbosity tipPoint)
          <> ", block no: "
          <> showT (unBlockNo tipBlkNo)
    TraceDidntAdoptBlock slotNo _ ->
      const $
        "Didn't adopt forged block at slot " <> showT (unSlotNo slotNo)
    TraceForgedBlock slotNo _ blk _ ->
      const $
        "Forged block for slot " <> showT (unSlotNo slotNo) <> " " <> showT blk
    TraceForgedInvalidBlock slotNo _ reason ->
      const $
        "Forged invalid block for slot " <> showT (unSlotNo slotNo)
          <> ", reason: "
          <> showT reason
    TraceNodeIsLeader slotNo ->
      const $
        "Leading slot " <> showT (unSlotNo slotNo)
    TraceNodeNotLeader slotNo ->
      const $
        "Not leading slot " <> showT (unSlotNo slotNo)
    TraceNodeCannotLead slotNo reason ->
      const $
        "We are the leader for slot "
          <> showT (unSlotNo slotNo)
          <> ", but we cannot lead because: "
          <> showT reason
    TraceNoLedgerState slotNo _blk ->
      const $
        "No ledger state at slot " <> showT (unSlotNo slotNo)
    TraceNoLedgerView slotNo _ ->
      const $
        "No ledger view at slot " <> showT (unSlotNo slotNo)
    TraceStartLeadershipCheck slotNo ->
      const $
        "Testing for leadership at slot " <> showT (unSlotNo slotNo)

instance Transformable Text IO (TraceLocalTxSubmissionServerEvent blk) where
  trTransformer = trStructured

instance
  ( Condense (HeaderHash blk),
    LedgerSupportsProtocol blk,
    ToObject (Header blk)
  ) =>
  Transformable Text IO (ChainDB.TraceEvent blk)
  where
  trTransformer = trStructuredText

instance
  (Condense (HeaderHash blk), LedgerSupportsProtocol blk) =>
  HasTextFormatter (ChainDB.TraceEvent blk)
  where
  formatText = \case
    ChainDB.TraceAddBlockEvent ev -> case ev of
      ChainDB.IgnoreBlockOlderThanK pt ->
        const $
          "Ignoring block older than K: " <> condenseT pt
      ChainDB.IgnoreBlockAlreadyInVolDB pt -> \_o ->
        "Ignoring block already in DB: " <> condenseT pt
      ChainDB.IgnoreInvalidBlock pt _reason -> \_o ->
        "Ignoring previously seen invalid block: " <> condenseT pt
      ChainDB.AddedBlockToQueue pt sz -> \_o ->
        "Block added to queue: " <> condenseT pt <> " queue size " <> condenseT sz
      ChainDB.BlockInTheFuture pt slot -> \_o ->
        "Ignoring block from future: " <> condenseT pt <> ", slot " <> condenseT slot
      ChainDB.StoreButDontChange pt -> \_o ->
        "Ignoring block: " <> condenseT pt
      ChainDB.TryAddToCurrentChain pt -> \_o ->
        "Block fits onto the current chain: " <> condenseT pt
      ChainDB.TrySwitchToAFork pt _ -> \_o ->
        "Block fits onto some fork: " <> condenseT pt
      ChainDB.AddedToCurrentChain _ _ c -> \_o ->
        "Chain extended, new tip: " <> condenseT (AF.headPoint c)
      ChainDB.SwitchedToAFork _ _ c -> \_o ->
        "Switched to a fork, new tip: " <> condenseT (AF.headPoint c)
      ChainDB.AddBlockValidation ev' -> case ev' of
        ChainDB.InvalidBlock err pt -> \_o ->
          "Invalid block " <> condenseT pt <> ": " <> showT err
        ChainDB.InvalidCandidate c -> \_o ->
          "Invalid candidate " <> condenseT (AF.headPoint c)
        ChainDB.ValidCandidate c -> \_o ->
          "Valid candidate " <> condenseT (AF.headPoint c)
        ChainDB.CandidateContainsFutureBlocks c hdrs -> \_o ->
          "Candidate contains blocks from near future:  "
            <> condenseT (AF.headPoint c)
            <> ", slots "
            <> Text.intercalate ", " (map (condenseT . headerPoint) hdrs)
        ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs -> \_o ->
          "Candidate contains blocks from future exceeding clock skew limit: "
            <> condenseT (AF.headPoint c)
            <> ", slots "
            <> Text.intercalate ", " (map (condenseT . headerPoint) hdrs)
      ChainDB.AddedBlockToVolDB pt _ _ -> \_o ->
        "Chain added block " <> condenseT pt
      ChainDB.ChainSelectionForFutureBlock pt -> \_o ->
        "Chain selection run for block previously from future: " <> condenseT pt
    ChainDB.TraceLedgerReplayEvent ev -> case ev of
      LedgerDB.ReplayFromGenesis _replayTo -> \_o ->
        "Replaying ledger from genesis"
      LedgerDB.ReplayFromSnapshot snap tip' _replayTo -> \_o ->
        "Replaying ledger from snapshot " <> showT snap <> " at "
          <> condenseT tip'
      LedgerDB.ReplayedBlock pt replayTo -> \_o ->
        "Replayed block: slot " <> showT (realPointSlot pt) <> " of " <> showT (pointSlot replayTo)
    ChainDB.TraceLedgerEvent ev -> case ev of
      LedgerDB.TookSnapshot snap pt -> \_o ->
        "Took ledger snapshot " <> showT snap <> " at " <> condenseT pt
      LedgerDB.DeletedSnapshot snap -> \_o ->
        "Deleted old snapshot " <> showT snap
      LedgerDB.InvalidSnapshot snap failure -> \_o ->
        "Invalid snapshot " <> showT snap <> showT failure
    ChainDB.TraceCopyToImmDBEvent ev -> case ev of
      ChainDB.CopiedBlockToImmDB pt -> \_o ->
        "Copied block " <> condenseT pt <> " to the ImmutableDB"
      ChainDB.NoBlocksToCopyToImmDB -> \_o ->
        "There are no blocks to copy to the ImmutableDB"
    ChainDB.TraceGCEvent ev -> case ev of
      ChainDB.PerformedGC slot -> \_o ->
        "Performed a garbage collection for " <> condenseT slot
      ChainDB.ScheduledGC slot _difft -> \_o ->
        "Scheduled a garbage collection for " <> condenseT slot
    ChainDB.TraceOpenEvent ev -> case ev of
      ChainDB.OpenedDB immTip tip' -> \_o ->
        "Opened db with immutable tip at " <> condenseT immTip
          <> " and tip "
          <> condenseT tip'
      ChainDB.ClosedDB immTip tip' -> \_o ->
        "Closed db with immutable tip at " <> condenseT immTip
          <> " and tip "
          <> condenseT tip'
      ChainDB.OpenedImmDB immTip epoch -> \_o ->
        "Opened imm db with immutable tip at " <> condenseT immTip
          <> " and epoch "
          <> showT epoch
      ChainDB.OpenedVolDB -> \_o -> "Opened vol db"
      ChainDB.OpenedLgrDB -> \_o -> "Opened lgr db"
    ChainDB.TraceReaderEvent ev -> case ev of
      ChainDB.NewReader -> \_o -> "New reader was created"
      ChainDB.ReaderNoLongerInMem _ -> \_o -> "ReaderNoLongerInMem"
      ChainDB.ReaderSwitchToMem _ _ -> \_o -> "ReaderSwitchToMem"
      ChainDB.ReaderNewImmIterator _ _ -> \_o -> "ReaderNewImmIterator"
    ChainDB.TraceInitChainSelEvent ev -> case ev of
      ChainDB.InitChainSelValidation _ -> \_o -> "InitChainSelValidation"
    ChainDB.TraceIteratorEvent ev -> case ev of
      ChainDB.UnknownRangeRequested _ -> \_o -> "UnknownRangeRequested"
      ChainDB.BlockMissingFromVolDB _ -> \_o -> "BlockMissingFromVolDB"
      ChainDB.StreamFromImmDB _ _ -> \_o -> "StreamFromImmDB"
      ChainDB.StreamFromBoth _ _ _ -> \_o -> "StreamFromBoth"
      ChainDB.StreamFromVolDB _ _ _ -> \_o -> "StreamFromVolDB"
      ChainDB.BlockWasCopiedToImmDB _ -> \_o -> "BlockWasCopiedToImmDB"
      ChainDB.BlockGCedFromVolDB _ -> \_o -> "BlockGCedFromVolDB"
      ChainDB.SwitchBackToVolDB -> \_o -> "SwitchBackToVolDB"
    ChainDB.TraceImmDBEvent ev -> \_o -> Text.append "TraceImmDBEvent" (showT ev)
    ChainDB.TraceVolDBEvent ev -> \_o -> Text.append "TraceVolDBEvent " (showT ev)

--

-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.
instance ToObject BftValidationErr where
  toObject _verb (BftInvalidSignature err) =
    mkObject
      [ "kind" .= String "BftInvalidSignature",
        "error" .= String (pack err)
      ]

instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject ["kind" .= String "snapshot"]
  toObject MaximalVerbosity snap =
    mkObject
      [ "kind" .= String "snapshot",
        "snapshot" .= String (pack $ show snap)
      ]

instance
  ( StandardHash blk,
    ToObject (LedgerError blk),
    ToObject (OtherHeaderEnvelopeError blk),
    ToObject (ValidationErr (BlockProtocol blk))
  ) =>
  ToObject (ExtValidationError blk)
  where
  toObject verb (ExtValidationErrorLedger err) = toObject verb err
  toObject verb (ExtValidationErrorHeader err) = toObject verb err

instance
  ( StandardHash blk,
    ToObject (OtherHeaderEnvelopeError blk)
  ) =>
  ToObject (HeaderEnvelopeError blk)
  where
  toObject _verb (UnexpectedBlockNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedBlockNo",
        "expected" .= condense expect,
        "actual" .= condense act
      ]
  toObject _verb (UnexpectedSlotNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedSlotNo",
        "expected" .= condense expect,
        "actual" .= condense act
      ]
  toObject _verb (UnexpectedPrevHash expect act) =
    mkObject
      [ "kind" .= String "UnexpectedPrevHash",
        "expected" .= String (pack $ show expect),
        "actual" .= String (pack $ show act)
      ]
  toObject verb (OtherHeaderEnvelopeError err) =
    toObject verb err

instance
  ( StandardHash blk,
    ToObject (ValidationErr (BlockProtocol blk)),
    ToObject (OtherHeaderEnvelopeError blk)
  ) =>
  ToObject (HeaderError blk)
  where
  toObject verb (HeaderProtocolError err) =
    mkObject
      [ "kind" .= String "HeaderProtocolError",
        "error" .= toObject verb err
      ]
  toObject verb (HeaderEnvelopeError err) =
    mkObject
      [ "kind" .= String "HeaderEnvelopeError",
        "error" .= toObject verb err
      ]

instance
  ( Condense (HeaderHash blk),
    StandardHash blk,
    ToObject (LedgerError blk),
    ToObject (OtherHeaderEnvelopeError blk),
    ToObject (ValidationErr (BlockProtocol blk))
  ) =>
  ToObject (ChainDB.InvalidBlockReason blk)
  where
  toObject verb (ChainDB.ValidationError extvalerr) =
    mkObject
      [ "kind" .= String "ValidationError",
        "error" .= toObject verb extvalerr
      ]
  toObject verb (ChainDB.InFutureExceedsClockSkew point) =
    mkObject
      [ "kind" .= String "InFutureExceedsClockSkew",
        "point" .= toObject verb point
      ]

instance
  Condense (HeaderHash blk) =>
  ToObject (RealPoint blk)
  where
  toObject verb p =
    mkObject $
      [ "kind" .= String "Point",
        "slot" .= unSlotNo (realPointSlot p)
      ]
        ++ ["hash" .= condense (realPointHash p) | verb == MaximalVerbosity]

instance
  ( Condense (HeaderHash blk),
    LedgerSupportsProtocol blk,
    ToObject (Header blk)
  ) =>
  ToObject (ChainDB.TraceEvent blk)
  where
  toObject verb (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK pt ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.IgnoreBlockOlderThanK",
          "block" .= toObject verb pt
        ]
    ChainDB.IgnoreBlockAlreadyInVolDB pt ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.IgnoreBlockAlreadyInVolDB",
          "block" .= toObject verb pt
        ]
    ChainDB.IgnoreInvalidBlock pt reason ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.IgnoreInvalidBlock",
          "block" .= toObject verb pt,
          "reason" .= show reason
        ]
    ChainDB.AddedBlockToQueue pt sz ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.AddedBlockToQueue",
          "block" .= toObject verb pt,
          "queueSize" .= toJSON sz
        ]
    ChainDB.BlockInTheFuture pt slot ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.BlockInTheFuture",
          "block" .= toObject verb pt,
          "slot" .= toObject verb slot
        ]
    ChainDB.StoreButDontChange pt ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.StoreButDontChange",
          "block" .= toObject verb pt
        ]
    ChainDB.TryAddToCurrentChain pt ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.TryAddToCurrentChain",
          "block" .= toObject verb pt
        ]
    ChainDB.TrySwitchToAFork pt _ ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork",
          "block" .= toObject verb pt
        ]
    ChainDB.AddedToCurrentChain _ base extended ->
      mkObject $
        [ "kind" .= String "TraceAddBlockEvent.AddedToCurrentChain",
          "newtip" .= showPoint verb (AF.headPoint extended)
        ]
          ++ [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain base extended)
               | verb == MaximalVerbosity
             ]
    ChainDB.SwitchedToAFork _ old new ->
      mkObject $
        [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork",
          "newtip" .= showPoint verb (AF.headPoint new)
        ]
          ++ [ "headers" .= toJSON (toObject verb `map` addedHdrsNewChain old new)
               | verb == MaximalVerbosity
             ]
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidBlock",
            "block" .= toObject verb pt,
            "error" .= show err
          ]
      ChainDB.InvalidCandidate c ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.InvalidCandidate",
            "block" .= showPoint verb (AF.headPoint c)
          ]
      ChainDB.ValidCandidate c ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.ValidCandidate",
            "block" .= showPoint verb (AF.headPoint c)
          ]
      ChainDB.CandidateContainsFutureBlocks c hdrs ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks",
            "block" .= showPoint verb (AF.headPoint c),
            "headers" .= map (showPoint verb . headerPoint) hdrs
          ]
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew",
            "block" .= showPoint verb (AF.headPoint c),
            "headers" .= map (showPoint verb . headerPoint) hdrs
          ]
    ChainDB.AddedBlockToVolDB pt (BlockNo bn) _ ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.AddedBlockToVolDB",
          "block" .= toObject verb pt,
          "blockNo" .= show bn
        ]
    ChainDB.ChainSelectionForFutureBlock pt ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.ChainSelectionForFutureBlock",
          "block" .= toObject verb pt
        ]
    where
      addedHdrsNewChain ::
        (AF.AnchoredFragment (Header blk)) ->
        (AF.AnchoredFragment (Header blk)) ->
        [Header blk]
      addedHdrsNewChain fro to_ =
        case AF.intersect fro to_ of
          Just (_, _, _, s2 :: AF.AnchoredFragment (Header blk)) ->
            AF.toOldestFirst s2
          Nothing -> [] -- No sense to do validation here.
  toObject MinimalVerbosity (ChainDB.TraceLedgerReplayEvent _ev) = emptyObject -- no output
  toObject verb (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis _replayTo ->
      mkObject ["kind" .= String "TraceLedgerReplayEvent.ReplayFromGenesis"]
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
      mkObject
        [ "kind" .= String "TraceLedgerReplayEvent.ReplayFromSnapshot",
          "snapshot" .= toObject verb snap,
          "tip" .= show tip'
        ]
    LedgerDB.ReplayedBlock pt replayTo ->
      mkObject
        [ "kind" .= String "TraceLedgerReplayEvent.ReplayedBlock",
          "slot" .= unSlotNo (realPointSlot pt),
          "tip" .= withOrigin 0 unSlotNo (pointSlot replayTo)
        ]
  toObject MinimalVerbosity (ChainDB.TraceLedgerEvent _ev) = emptyObject -- no output
  toObject verb (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot snap pt ->
      mkObject
        [ "kind" .= String "TraceLedgerEvent.TookSnapshot",
          "snapshot" .= toObject verb snap,
          "tip" .= show pt
        ]
    LedgerDB.DeletedSnapshot snap ->
      mkObject
        [ "kind" .= String "TraceLedgerEvent.DeletedSnapshot",
          "snapshot" .= toObject verb snap
        ]
    LedgerDB.InvalidSnapshot snap failure ->
      mkObject
        [ "kind" .= String "TraceLedgerEvent.InvalidSnapshot",
          "snapshot" .= toObject verb snap,
          "failure" .= show failure
        ]
  toObject verb (ChainDB.TraceCopyToImmDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmDB pt ->
      mkObject
        [ "kind" .= String "TraceCopyToImmDBEvent.CopiedBlockToImmDB",
          "slot" .= toObject verb pt
        ]
    ChainDB.NoBlocksToCopyToImmDB ->
      mkObject ["kind" .= String "TraceCopyToImmDBEvent.NoBlocksToCopyToImmDB"]
  toObject verb (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC slot ->
      mkObject
        [ "kind" .= String "TraceGCEvent.PerformedGC",
          "slot" .= toObject verb slot
        ]
    ChainDB.ScheduledGC slot difft ->
      mkObject $
        [ "kind" .= String "TraceGCEvent.ScheduledGC",
          "slot" .= toObject verb slot
        ]
          <> ["difft" .= String ((pack . show) difft) | verb >= MaximalVerbosity]
  toObject verb (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB immTip tip' ->
      mkObject
        [ "kind" .= String "TraceOpenEvent.OpenedDB",
          "immtip" .= toObject verb immTip,
          "tip" .= toObject verb tip'
        ]
    ChainDB.ClosedDB immTip tip' ->
      mkObject
        [ "kind" .= String "TraceOpenEvent.ClosedDB",
          "immtip" .= toObject verb immTip,
          "tip" .= toObject verb tip'
        ]
    ChainDB.OpenedImmDB immTip epoch ->
      mkObject
        [ "kind" .= String "TraceOpenEvent.OpenedImmDB",
          "immtip" .= toObject verb immTip,
          "epoch" .= String ((pack . show) epoch)
        ]
    ChainDB.OpenedVolDB ->
      mkObject ["kind" .= String "TraceOpenEvent.OpenedVolDB"]
    ChainDB.OpenedLgrDB ->
      mkObject ["kind" .= String "TraceOpenEvent.OpenedLgrDB"]
  toObject _verb (ChainDB.TraceReaderEvent ev) = case ev of
    ChainDB.NewReader ->
      mkObject ["kind" .= String "TraceReaderEvent.NewReader"]
    ChainDB.ReaderNoLongerInMem _ ->
      mkObject ["kind" .= String "TraceReaderEvent.ReaderNoLongerInMem"]
    ChainDB.ReaderSwitchToMem _ _ ->
      mkObject ["kind" .= String "TraceReaderEvent.ReaderSwitchToMem"]
    ChainDB.ReaderNewImmIterator _ _ ->
      mkObject ["kind" .= String "TraceReaderEvent.ReaderNewImmIterator"]
  toObject _verb (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation _ ->
      mkObject ["kind" .= String "InitChainSelValidation"]
  toObject _verb (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.StreamFromVolDB _ _ _ ->
      mkObject ["kind" .= String "StreamFromVolDB"]
    _ -> emptyObject -- TODO add more iterator events
  toObject _verb (ChainDB.TraceImmDBEvent _ev) =
    mkObject ["kind" .= String "TraceImmDBEvent"]
  toObject _verb (ChainDB.TraceVolDBEvent _ev) =
    mkObject ["kind" .= String "TraceVolDBEvent"]

instance ToObject (TraceBlockFetchServerEvent blk) where
  toObject _verb _ =
    mkObject ["kind" .= String "TraceBlockFetchServerEvent"]

instance
  (Condense (HeaderHash blk), LedgerSupportsProtocol blk) =>
  ToObject (TraceChainSyncClientEvent blk)
  where
  toObject verb ev = case ev of
    TraceDownloadedHeader pt ->
      mkObject
        [ "kind" .= String "ChainSyncClientEvent.TraceDownloadedHeader",
          "block" .= toObject verb (headerPoint pt)
        ]
    TraceRolledBack tip ->
      mkObject
        [ "kind" .= String "ChainSyncClientEvent.TraceRolledBack",
          "tip" .= toObject verb tip
        ]
    TraceException exc ->
      mkObject
        [ "kind" .= String "ChainSyncClientEvent.TraceException",
          "exception" .= String (pack $ show exc)
        ]
    TraceFoundIntersection _ _ _ ->
      mkObject ["kind" .= String "ChainSyncClientEvent.TraceFoundIntersection"]

instance
  Condense (HeaderHash blk) =>
  ToObject (TraceChainSyncServerEvent blk)
  where
  toObject verb ev = case ev of
    TraceChainSyncServerRead tip (AddBlock hdr) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock",
          "tip" .= (String (pack $ showTip verb tip)),
          "addedBlock" .= (String (pack $ condense hdr))
        ]
    TraceChainSyncServerRead tip (RollBack pt) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.RollBack",
          "tip" .= (String (pack $ showTip verb tip)),
          "rolledBackBlock" .= (String (pack $ showPoint verb pt))
        ]
    TraceChainSyncServerReadBlocked tip (AddBlock hdr) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock",
          "tip" .= (String (pack $ showTip verb tip)),
          "addedBlock" .= (String (pack $ condense hdr))
        ]
    TraceChainSyncServerReadBlocked tip (RollBack pt) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.RollBack",
          "tip" .= (String (pack $ showTip verb tip)),
          "rolledBackBlock" .= (String (pack $ showPoint verb pt))
        ]

instance
  ( Show (ApplyTxErr blk),
    ToObject (ApplyTxErr blk),
    ToObject (GenTx blk),
    ToJSON (GenTxId blk)
  ) =>
  ToObject (TraceEventMempool blk)
  where
  toObject verb (TraceMempoolAddedTx tx _mpSzBefore mpSzAfter) =
    mkObject
      [ "kind" .= String "TraceMempoolAddedTx",
        "tx" .= toObject verb tx,
        "mempoolSize" .= toObject verb mpSzAfter
      ]
  toObject verb (TraceMempoolRejectedTx tx txApplyErr mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRejectedTx",
        "err" .= toObject verb txApplyErr,
        "tx" .= toObject verb tx,
        "mempoolSize" .= toObject verb mpSz
      ]
  toObject verb (TraceMempoolRemoveTxs txs mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolRemoveTxs",
        "txs" .= map (toObject verb) txs,
        "mempoolSize" .= toObject verb mpSz
      ]
  toObject verb (TraceMempoolManuallyRemovedTxs txs0 txs1 mpSz) =
    mkObject
      [ "kind" .= String "TraceMempoolManuallyRemovedTxs",
        "txsRemoved" .= txs0,
        "txsInvalidated" .= map (toObject verb) txs1,
        "mempoolSize" .= toObject verb mpSz
      ]

instance
  (Show (GenTxId blk), Show (ApplyTxErr blk), Show (GenTx blk)) =>
  HasTextFormatter (TraceEventMempool blk)
  where
  formatText a _ = pack $ show a

instance ToObject MempoolSize where
  toObject _verb MempoolSize {msNumTxs, msNumBytes} =
    mkObject
      [ "numTxs" .= msNumTxs,
        "bytes" .= msNumBytes
      ]

instance HasTextFormatter () where
  formatText _ = pack . show . toList

-- ForgeState default value = ()
instance Transformable Text IO () where
  trTransformer = trStructuredText

instance
  ToObject (ChainIndepState (BlockProtocol blk)) =>
  ToObject (ForgeState blk)
  where
  toObject verb ForgeState {chainIndepState, extraForgeState = _} =
    -- We assume there's nothing interesting in the extraForgeState
    toObject verb chainIndepState

instance
  ( tx ~ GenTx blk,
    Condense (HeaderHash blk),
    HasTxId tx,
    RunNode blk,
    Show (TxId tx),
    ToObject (LedgerError blk),
    ToObject (OtherHeaderEnvelopeError blk),
    ToObject (ValidationErr (BlockProtocol blk)),
    ToObject (CannotLead (BlockProtocol blk))
  ) =>
  ToObject (TraceForgeEvent blk)
  where
  toObject MaximalVerbosity (TraceAdoptedBlock slotNo blk txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock",
        "slot" .= toJSON (unSlotNo slotNo),
        "block hash" .= (condense $ blockHash blk),
        "block size" .= toJSON (nodeBlockFetchSize (getHeader blk)),
        "tx ids" .= toJSON (map (show . txId) txs)
      ]
  toObject _verb (TraceAdoptedBlock slotNo blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock",
        "slot" .= toJSON (unSlotNo slotNo),
        "block hash" .= (condense $ blockHash blk),
        "block size" .= toJSON (nodeBlockFetchSize (getHeader blk))
      ]
  toObject _verb (TraceBlockFromFuture currentSlot tip) =
    mkObject
      [ "kind" .= String "TraceBlockFromFuture",
        "current slot" .= toJSON (unSlotNo currentSlot),
        "tip" .= toJSON (unSlotNo tip)
      ]
  toObject verb (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    mkObject
      [ "kind" .= String "TraceSlotIsImmutable",
        "slot" .= toJSON (unSlotNo slotNo),
        "tip" .= showPoint verb tipPoint,
        "tipBlockNo" .= toJSON (unBlockNo tipBlkNo)
      ]
  toObject _verb (TraceDidntAdoptBlock slotNo _) =
    mkObject
      [ "kind" .= String "TraceDidntAdoptBlock",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceForgedBlock slotNo _ _ _) =
    mkObject
      [ "kind" .= String "TraceForgedBlock",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject verb (TraceForgedInvalidBlock slotNo _ reason) =
    mkObject
      [ "kind" .= String "TraceForgedInvalidBlock",
        "slot" .= toJSON (unSlotNo slotNo),
        "reason" .= toObject verb reason
      ]
  toObject _verb (TraceNodeIsLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeIsLeader",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNodeNotLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeNotLeader",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject verb (TraceNodeCannotLead slotNo reason) =
    mkObject
      [ "kind" .= String "TraceNodeCannotLead",
        "slot" .= toJSON (unSlotNo slotNo),
        "reason" .= toObject verb reason
      ]
  toObject _verb (TraceNoLedgerState slotNo _blk) =
    mkObject
      [ "kind" .= String "TraceNoLedgerState",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNoLedgerView slotNo _) =
    mkObject
      [ "kind" .= String "TraceNoLedgerView",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceStartLeadershipCheck slotNo) =
    mkObject
      [ "kind" .= String "TraceStartLeadershipCheck",
        "slot" .= toJSON (unSlotNo slotNo)
      ]

instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject ["kind" .= String "TraceLocalTxSubmissionServerEvent"]

-- | Tracing wrapper which includes current tip in the logs (thus it requires
-- it from the context).
--
-- TODO: this should be moved to `ouroboros-consensus`.  Running in a seprate
-- STM transaction we risk reporting  wrong tip.
data WithTip blk a = WithTip (Point blk) a

showTip ::
  Condense (HeaderHash blk) =>
  TracingVerbosity ->
  Tip blk ->
  String
showTip verb = showPoint verb . getTipPoint

showPoint ::
  Condense (HeaderHash blk) =>
  TracingVerbosity ->
  Point blk ->
  String
showPoint verb pt =
  case pt of
    GenesisPoint -> "genesis (origin)"
    BlockPoint slot h -> trim (condense h) ++ "@" ++ condense slot
  where
    trim :: [a] -> [a]
    trim = case verb of
      MinimalVerbosity -> take 7
      NormalVerbosity -> take 7
      MaximalVerbosity -> id

--

-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@

--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation NtC.HandshakeTr

instance HasSeverityAnnotation NtC.HandshakeTr where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation NtN.HandshakeTr

instance HasSeverityAnnotation NtN.HandshakeTr where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation NtN.AcceptConnectionsPolicyTrace

instance HasSeverityAnnotation NtN.AcceptConnectionsPolicyTrace where
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionHardLimit {} = Warning

instance HasPrivacyAnnotation (TraceFetchClientState header)

instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceSendRecv a)

instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation a => HasPrivacyAnnotation (TraceLabelPeer peer a)

instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelPeer peer a)

instance HasPrivacyAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]

instance HasSeverityAnnotation [TraceLabelPeer peer (FetchDecision [Point header])] where
  getSeverityAnnotation [] = Debug
  getSeverityAnnotation xs =
    maximum $ map (\(TraceLabelPeer _ a) -> fetchDecisionSeverity a) xs
    where
      fetchDecisionSeverity :: FetchDecision a -> Severity
      fetchDecisionSeverity fd =
        case fd of
          Left FetchDeclineChainNotPlausible -> Debug
          Left FetchDeclineChainNoIntersection -> Notice
          Left FetchDeclineAlreadyFetched -> Debug
          Left FetchDeclineInFlightThisPeer -> Debug
          Left FetchDeclineInFlightOtherPeer -> Debug
          Left FetchDeclinePeerShutdown -> Info
          Left FetchDeclinePeerSlow -> Info
          Left FetchDeclineReqsInFlightLimit {} -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {} -> Info
          Left FetchDeclineConcurrencyLimit {} -> Info
          Right _ -> Info

instance HasPrivacyAnnotation (TraceTxSubmissionInbound txid tx)

instance HasSeverityAnnotation (TraceTxSubmissionInbound txid tx) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceTxSubmissionOutbound txid tx)

instance HasSeverityAnnotation (TraceTxSubmissionOutbound txid tx) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (WithAddr addr ErrorPolicyTrace)

instance HasSeverityAnnotation (WithAddr addr ErrorPolicyTrace) where
  getSeverityAnnotation (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error

instance HasPrivacyAnnotation (WithDomainName DnsTrace)

instance HasSeverityAnnotation (WithDomainName DnsTrace) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    DnsTraceLookupException {} -> Error
    DnsTraceLookupAError {} -> Error
    DnsTraceLookupAAAAError {} -> Error
    DnsTraceLookupIPv6First -> Debug
    DnsTraceLookupIPv4First -> Debug
    DnsTraceLookupAResult {} -> Debug
    DnsTraceLookupAAAAResult {} -> Debug

instance HasPrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))

instance HasSeverityAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException _ e ->
      case fromException $ SomeException e of
        Just (_ :: SubscriberError) -> Debug
        Nothing -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
      case fromException $ SomeException e of
        Just (_ :: SubscriberError) -> Debug
        Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug

instance HasPrivacyAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))

instance HasSeverityAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  getSeverityAnnotation (WithIPList _ _ ev) = case ev of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess -> Info
      ConnectSuccessLast -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException _ e ->
      case fromException $ SomeException e of
        Just (_ :: SubscriberError) -> Debug
        Nothing -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
      case fromException $ SomeException e of
        Just (_ :: SubscriberError) -> Debug
        Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info

instance HasPrivacyAnnotation (Identity (SubscriptionTrace LocalAddress))

instance HasSeverityAnnotation (Identity (SubscriptionTrace LocalAddress)) where
  getSeverityAnnotation (Identity ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Notice
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Notice
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Notice
    SubscriptionTraceRestart {} -> Notice
    SubscriptionTraceConnectionExist {} -> Debug
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug

instance Transformable Text IO (Identity (SubscriptionTrace LocalAddress)) where
  trTransformer = trStructuredText

instance HasTextFormatter (Identity (SubscriptionTrace LocalAddress)) where
  formatText _ = pack . show . toList

instance ToObject (Identity (SubscriptionTrace LocalAddress)) where
  toObject _verb (Identity ev) =
    mkObject
      [ "kind" .= ("SubscriptionTrace" :: String),
        "event" .= show ev
      ]

instance HasPrivacyAnnotation (WithMuxBearer peer MuxTrace)

instance HasSeverityAnnotation (WithMuxBearer peer MuxTrace) where
  getSeverityAnnotation (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart -> Debug
    MuxTraceRecvHeaderEnd {} -> Debug
    MuxTraceRecvStart {} -> Debug
    MuxTraceRecvEnd {} -> Debug
    MuxTraceSendStart {} -> Debug
    MuxTraceSendEnd -> Debug
    MuxTraceState {} -> Info
    MuxTraceCleanExit {} -> Notice
    MuxTraceExceptionExit {} -> Notice
    MuxTraceChannelRecvStart {} -> Debug
    MuxTraceChannelRecvEnd {} -> Debug
    MuxTraceChannelSendStart {} -> Debug
    MuxTraceChannelSendEnd {} -> Debug
    MuxTraceHandshakeStart -> Debug
    MuxTraceHandshakeClientEnd {} -> Info
    MuxTraceHandshakeServerEnd -> Debug
    MuxTraceHandshakeClientError {} -> Error
    MuxTraceHandshakeServerError {} -> Error
    MuxTraceRecvDeltaQObservation {} -> Debug
    MuxTraceRecvDeltaQSample {} -> Debug
    MuxTraceSDUReadTimeoutException -> Notice
    MuxTraceSDUWriteTimeoutException -> Notice
    MuxTraceStartEagerly _ _ -> Debug
    MuxTraceStartOnDemand _ _ -> Debug
    MuxTraceStartedOnDemand _ _ -> Debug
    MuxTraceShutdown -> Debug

--

-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.
instance Transformable Text IO NtN.HandshakeTr where
  trTransformer = trStructuredText

instance HasTextFormatter NtN.HandshakeTr where
  formatText a _ = showT a

instance Transformable Text IO NtC.HandshakeTr where
  trTransformer = trStructuredText

instance HasTextFormatter NtC.HandshakeTr where
  formatText a _ = showT a

instance Transformable Text IO NtN.AcceptConnectionsPolicyTrace where
  trTransformer = trStructuredText

instance HasTextFormatter NtN.AcceptConnectionsPolicyTrace where
  formatText a _ = showT a

instance
  Show peer =>
  Transformable Text IO [TraceLabelPeer peer (FetchDecision [Point header])]
  where
  trTransformer = trStructuredText

instance HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText _ = pack . show . toList

instance
  (Show peer, HasPrivacyAnnotation a, HasSeverityAnnotation a, ToObject a) =>
  Transformable Text IO (TraceLabelPeer peer a)
  where
  trTransformer = trStructuredText

instance HasTextFormatter (TraceLabelPeer peer a) where
  formatText _ = pack . show . toList

instance Transformable Text IO (TraceTxSubmissionInbound txid tx) where
  trTransformer = trStructuredText

instance HasTextFormatter (TraceTxSubmissionInbound txid tx) where
  formatText _ = pack . show . toList

instance
  (Show tx, Show txid) =>
  Transformable Text IO (TraceTxSubmissionOutbound txid tx)
  where
  trTransformer = trStructuredText

instance
  (Show tx, Show txid) =>
  HasTextFormatter (TraceTxSubmissionOutbound txid tx)
  where
  formatText a _ = showT a

instance Show addr => Transformable Text IO (WithAddr addr ErrorPolicyTrace) where
  trTransformer = trStructuredText

instance HasTextFormatter (WithAddr addr ErrorPolicyTrace) where
  formatText _ = pack . show . toList

instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText

instance HasTextFormatter (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  formatText _ = pack . show . toList

instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer = trStructuredText

instance HasTextFormatter (WithDomainName DnsTrace) where
  formatText _ = pack . show . toList

instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText

instance HasTextFormatter (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  formatText _ = pack . show . toList

instance
  (Show peer) =>
  Transformable Text IO (WithMuxBearer peer MuxTrace)
  where
  trTransformer = trStructuredText

instance
  (Show peer) =>
  HasTextFormatter (WithMuxBearer peer MuxTrace)
  where
  formatText (WithMuxBearer peer ev) = \_o ->
    "Bearer on " <> pack (show peer)
      <> " event: "
      <> pack (show ev)

--

-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.
instance
  ( Condense (HeaderHash blk),
    Condense (TxId (GenTx blk)),
    HasHeader blk,
    RunNode blk,
    HasTxs blk
  ) =>
  ToObject (AnyMessage (BlockFetch blk))
  where
  toObject MaximalVerbosity (AnyMessage (MsgBlock blk)) =
    mkObject
      [ "kind" .= String "MsgBlock",
        "block hash" .= (condense $ blockHash blk),
        "block size" .= toJSON (nodeBlockFetchSize (getHeader blk)),
        "tx ids" .= toJSON (presentTx <$> extractTxs blk)
      ]
    where
      presentTx :: GenTx blk -> Value
      presentTx = String . pack . condense . txId
  toObject _v (AnyMessage (MsgBlock blk)) =
    mkObject
      [ "kind" .= String "MsgBlock",
        "block hash" .= (condense $ blockHash blk),
        "block size" .= toJSON (nodeBlockFetchSize (getHeader blk))
      ]
  toObject _v (AnyMessage MsgRequestRange {}) =
    mkObject ["kind" .= String "MsgRequestRange"]
  toObject _v (AnyMessage MsgStartBatch {}) =
    mkObject ["kind" .= String "MsgStartBatch"]
  toObject _v (AnyMessage MsgNoBlocks {}) =
    mkObject ["kind" .= String "MsgNoBlocks"]
  toObject _v (AnyMessage MsgBatchDone {}) =
    mkObject ["kind" .= String "MsgBatchDone"]
  toObject _v (AnyMessage MsgClientDone {}) =
    mkObject ["kind" .= String "MsgClientDone"]

instance ToObject (AnyMessage (LocalStateQuery blk query)) where
  toObject _verb (AnyMessage LocalStateQuery.MsgAcquire {}) =
    mkObject ["kind" .= String "MsgAcquire"]
  toObject _verb (AnyMessage LocalStateQuery.MsgAcquired {}) =
    mkObject ["kind" .= String "MsgAcquired"]
  toObject _verb (AnyMessage LocalStateQuery.MsgFailure {}) =
    mkObject ["kind" .= String "MsgFailure"]
  toObject _verb (AnyMessage LocalStateQuery.MsgQuery {}) =
    mkObject ["kind" .= String "MsgQuery"]
  toObject _verb (AnyMessage LocalStateQuery.MsgResult {}) =
    mkObject ["kind" .= String "MsgResult"]
  toObject _verb (AnyMessage LocalStateQuery.MsgRelease {}) =
    mkObject ["kind" .= String "MsgRelease"]
  toObject _verb (AnyMessage LocalStateQuery.MsgReAcquire {}) =
    mkObject ["kind" .= String "MsgReAcquire"]
  toObject _verb (AnyMessage LocalStateQuery.MsgDone {}) =
    mkObject ["kind" .= String "MsgDone"]

instance ToObject (AnyMessage (LocalTxSubmission tx err)) where
  toObject _verb (AnyMessage LocalTxSub.MsgSubmitTx {}) =
    mkObject ["kind" .= String "MsgSubmitTx"]
  toObject _verb (AnyMessage LocalTxSub.MsgAcceptTx {}) =
    mkObject ["kind" .= String "MsgAcceptTx"]
  toObject _verb (AnyMessage LocalTxSub.MsgRejectTx {}) =
    mkObject ["kind" .= String "MsgRejectTx"]
  toObject _verb (AnyMessage LocalTxSub.MsgDone {}) =
    mkObject ["kind" .= String "MsgDone"]

instance ToObject (AnyMessage (ChainSync blk tip)) where
  toObject _verb (AnyMessage ChainSync.MsgRequestNext {}) =
    mkObject ["kind" .= String "MsgRequestNext"]
  toObject _verb (AnyMessage ChainSync.MsgAwaitReply {}) =
    mkObject ["kind" .= String "MsgAwaitReply"]
  toObject _verb (AnyMessage ChainSync.MsgRollForward {}) =
    mkObject ["kind" .= String "MsgRollForward"]
  toObject _verb (AnyMessage ChainSync.MsgRollBackward {}) =
    mkObject ["kind" .= String "MsgRollBackward"]
  toObject _verb (AnyMessage ChainSync.MsgFindIntersect {}) =
    mkObject ["kind" .= String "MsgFindIntersect"]
  toObject _verb (AnyMessage ChainSync.MsgIntersectFound {}) =
    mkObject ["kind" .= String "MsgIntersectFound"]
  toObject _verb (AnyMessage ChainSync.MsgIntersectNotFound {}) =
    mkObject ["kind" .= String "MsgIntersectNotFound"]
  toObject _verb (AnyMessage ChainSync.MsgDone {}) =
    mkObject ["kind" .= String "MsgDone"]

instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mkObject
      [ "kind" .= String "FetchDecision declined",
        "declined" .= String (pack $ show $ decline)
      ]
  toObject _verb (Right results) =
    mkObject
      [ "kind" .= String "FetchDecision results",
        "length" .= String (pack $ show $ length results)
      ]

instance ToObject NtC.HandshakeTr where
  toObject _verb (WithMuxBearer b ev) =
    mkObject
      [ "kind" .= String "LocalHandshakeTrace",
        "bearer" .= show b,
        "event" .= show ev
      ]

instance ToObject NtN.HandshakeTr where
  toObject _verb (WithMuxBearer b ev) =
    mkObject
      [ "kind" .= String "HandshakeTrace",
        "bearer" .= show b,
        "event" .= show ev
      ]

instance ToObject NtN.AcceptConnectionsPolicyTrace where
  toObject _verb (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
    mkObject
      [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting",
        "delay" .= show delay,
        "numberOfConnection" .= show numOfConnections
      ]
  toObject _verb (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
    mkObject
      [ "kind" .= String "ServerTraceAcceptConnectionHardLimit",
        "softLimit" .= show softLimit
      ]

instance
  (Show txid, Show tx) =>
  ToObject (AnyMessage (TxSubmission txid tx))
  where
  toObject _verb (AnyMessage (MsgRequestTxs txids)) =
    mkObject
      [ "kind" .= String "MsgRequestTxs",
        "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (AnyMessage (MsgReplyTxs txs)) =
    mkObject
      [ "kind" .= String "MsgReplyTxs",
        "txs" .= String (pack $ show txs)
      ]
  toObject _verb (AnyMessage (MsgRequestTxIds _ _ _)) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      ]
  toObject _verb (AnyMessage (MsgReplyTxIds txIds)) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds",
        "txIds" .= String (showT txIds)
      ]
  toObject _verb (AnyMessage MsgDone) =
    mkObject
      [ "kind" .= String "MsgDone"
      ]
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  toObject _verb (AnyMessage _) =
    mkObject
      ["kind" .= String "MsgKThxBye"]

instance
  Condense (HeaderHash blk) =>
  ToObject (Point blk)
  where
  toObject MinimalVerbosity p = toObject NormalVerbosity p
  toObject verb p =
    mkObject
      [ "kind" .= String "Tip", --TODO: why is this a Tip not a Point?
        "tip" .= showPoint verb p
      ]

instance ToObject SlotNo where
  toObject _verb slot =
    mkObject
      [ "kind" .= String "SlotNo",
        "slot" .= toJSON (unSlotNo slot)
      ]

instance ToObject (TraceFetchClientState header) where
  toObject _verb (AddedFetchRequest {}) =
    mkObject ["kind" .= String "AddedFetchRequest"]
  toObject _verb (AcknowledgedFetchRequest {}) =
    mkObject ["kind" .= String "AcknowledgedFetchRequest"]
  toObject _verb (CompletedBlockFetch {}) =
    mkObject ["kind" .= String "CompletedBlockFetch"]
  toObject _verb (CompletedFetchBatch {}) =
    mkObject ["kind" .= String "CompletedFetchBatch"]
  toObject _verb (StartedFetchBatch {}) =
    mkObject ["kind" .= String "StartedFetchBatch"]
  toObject _verb (RejectedFetchBatch {}) =
    mkObject ["kind" .= String "RejectedFetchBatch"]

instance
  Show peer =>
  ToObject [TraceLabelPeer peer (FetchDecision [Point header])]
  where
  toObject MinimalVerbosity _ = emptyObject
  toObject _ [] = emptyObject
  toObject _ xs =
    mkObject
      [ "kind" .= String "PeersFetch",
        "peers"
          .= toJSON
            (foldl' (\acc x -> toObject MaximalVerbosity x : acc) [] xs)
      ]

instance (Show peer, ToObject a) => ToObject (TraceLabelPeer peer a) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject ["peer" .= show peerid] <> toObject verb a

instance
  ToObject (AnyMessage ps) =>
  ToObject (TraceSendRecv ps)
  where
  toObject verb (TraceSendMsg m) =
    mkObject
      ["kind" .= String "Send", "msg" .= toObject verb m]
  toObject verb (TraceRecvMsg m) =
    mkObject
      ["kind" .= String "Recv", "msg" .= toObject verb m]

instance ToObject (TraceTxSubmissionInbound txid tx) where
  toObject _verb TraceTxSubmissionInbound =
    mkObject ["kind" .= String "TraceTxSubmissionInbound"]

instance
  (Show txid, Show tx) =>
  ToObject (TraceTxSubmissionOutbound txid tx)
  where
  toObject MaximalVerbosity (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs",
        "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundRecvMsgRequestTxs"
      ]
  toObject MaximalVerbosity (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs",
        "txs" .= String (pack $ show txs)
      ]
  toObject _verb (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mkObject
      [ "kind" .= String "TraceTxSubmissionOutboundSendMsgReplyTxs"
      ]

instance Show addr => ToObject (WithAddr addr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject
      [ "kind" .= String "ErrorPolicyTrace",
        "address" .= show addr,
        "event" .= show ev
      ]

instance ToObject (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mkObject
      [ "kind" .= String "WithIPList SubscriptionTrace",
        "localAddresses" .= show localAddresses,
        "dests" .= show dests,
        "event" .= show ev
      ]

instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mkObject
      [ "kind" .= String "DnsTrace",
        "domain" .= show dom,
        "event" .= show ev
      ]

instance ToObject (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mkObject
      [ "kind" .= String "SubscriptionTrace",
        "domain" .= show dom,
        "event" .= show ev
      ]

instance (Show peer) => ToObject (WithMuxBearer peer MuxTrace) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject
      [ "kind" .= String "MuxTrace",
        "bearer" .= show b,
        "event" .= show ev
      ]

-- | A bit of a weird one, but needed because some of the very general
-- consensus interfaces are sometimes instantaited to 'Void', when there are
-- no cases needed.
instance ToObject Void where
  toObject _verb x = case x of
