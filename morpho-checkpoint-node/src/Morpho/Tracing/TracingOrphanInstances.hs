{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Tracing.TracingOrphanInstances where

import Cardano.BM.Data.Tracer
  ( HasTextFormatter (..),
  )
import Cardano.BM.Tracing
  ( HasPrivacyAnnotation (..),
    HasSeverityAnnotation (..),
    Severity (..),
    TracingVerbosity (..),
  )
import Cardano.Crypto.DSIGN
import Cardano.Prelude hiding (show)
import Data.Text (pack)
import qualified Data.Text as Text
import Morpho.Ledger.Block
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Morpho.RPC.Abstract
import Morpho.Tracing.OrphanToJSONInstances ()
import Morpho.Tracing.Types
import Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket (SockAddr)
import Ouroboros.Consensus.Block
  ( headerPoint,
    realPointSlot,
  )
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol,
  )
import Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
  ( TraceBlockFetchServerEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientResult (..),
    InvalidBlockReason,
    TraceChainSyncClientEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( TraceChainSyncServerEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
  ( TraceLocalTxSubmissionServerEvent (..),
  )
import Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..), TraceLabelCreds (..))
import Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Types as VolatileDB
import Ouroboros.Consensus.Util.Condense (Condense, condense)
import Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState
  ( TraceFetchClientState (..),
    TraceLabelPeer (..),
  )
import Ouroboros.Network.BlockFetch.Decision
  ( FetchDecision,
    FetchDecline (..),
  )
import Ouroboros.Network.Diffusion
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
  ( ErrorPolicyTrace (..),
    TraceSendRecv (..),
    WithAddr (..),
  )
import qualified Ouroboros.Network.NodeToNode as NtN
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.Snocket (LocalAddress (..))
import Ouroboros.Network.Subscription
  ( ConnectResult (..),
    DnsTrace (..),
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

instance HasPrivacyAnnotation (TraceBlockchainTimeEvent t)

instance HasSeverityAnnotation (TraceBlockchainTimeEvent t) where
  getSeverityAnnotation (TraceStartTimeInTheFuture _ _) = Debug
  getSeverityAnnotation (TraceCurrentSlotUnknown _ _) = Debug
  getSeverityAnnotation (TraceSystemClockMovedBack _ _) = Warning

instance HasTextFormatter (TraceBlockchainTimeEvent t) where
  formatText ev _ = case ev of
    TraceStartTimeInTheFuture (SystemStart start) toWait ->
      "Waiting " <> showT toWait <> " until genesis start time at " <> showT start
    TraceSystemClockMovedBack _ _ -> "System clock moved back an acceptable time span"
    TraceCurrentSlotUnknown _ _ -> "Current slot is not yet known"

instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelCreds a) where
  getSeverityAnnotation (TraceLabelCreds _ a) = getSeverityAnnotation a

instance HasPrivacyAnnotation (TraceLabelCreds a)

instance (HashAlgorithm h, BftCrypto c) => HasTextFormatter (ExtractStateTrace h c) where
  formatText (MorphoStateTrace st) _ = pack $ "Current Ledger State: " ++ show st
  formatText (WontPushCheckpointTrace reason) _ =
    pack $ "Checkpoint won't be pushed: " ++ show reason
  formatText (VoteErrorTrace err) _ =
    pack $ "Error while trying to create a vote: " ++ show err

instance HasPrivacyAnnotation (ExtractStateTrace h c)

instance HasSeverityAnnotation (ExtractStateTrace h c) where
  getSeverityAnnotation MorphoStateTrace {} = Info
  getSeverityAnnotation WontPushCheckpointTrace {} = Debug
  getSeverityAnnotation VoteErrorTrace {} = Error

instance Show e => HasTextFormatter (RpcTrace e i o)

instance HasPrivacyAnnotation (RpcTrace e i o)

instance HasSeverityAnnotation e => HasSeverityAnnotation (RpcTrace e i o) where
  getSeverityAnnotation (RpcTrace _ _ RpcStart) = Info
  getSeverityAnnotation (RpcTrace GetLatestBlock _ (RpcSuccess Nothing)) = Notice
  getSeverityAnnotation (RpcTrace _ _ (RpcSuccess _)) = Info
  getSeverityAnnotation (RpcTrace _ _ (RpcEvent x)) = getSeverityAnnotation x

instance HasPrivacyAnnotation (TimeTravelError blk)

instance HasSeverityAnnotation (TimeTravelError blk) where
  getSeverityAnnotation (ChainNotLongEnough _ _) = Info
  getSeverityAnnotation (LedgerStateNotFoundAt _) = Error

instance (StandardHash blk) => HasTextFormatter (TimeTravelError blk)

instance HasPrivacyAnnotation (ChainDB.TraceEvent blk)

instance HasSeverityAnnotation (ChainDB.TraceEvent blk) where
  getSeverityAnnotation (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK {} -> Notice
    ChainDB.IgnoreBlockAlreadyInVolatileDB {} -> Debug
    ChainDB.IgnoreInvalidBlock {} -> Notice
    ChainDB.AddedBlockToQueue {} -> Debug
    ChainDB.BlockInTheFuture {} -> Info
    ChainDB.AddedBlockToVolatileDB {} -> Debug
    ChainDB.TryAddToCurrentChain {} -> Debug
    ChainDB.TrySwitchToAFork {} -> Debug
    ChainDB.StoreButDontChange {} -> Debug
    ChainDB.AddedToCurrentChain {} -> Info
    ChainDB.SwitchedToAFork {} -> Notice
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock {} -> Error
      ChainDB.InvalidCandidate {} -> Error
      ChainDB.ValidCandidate {} -> Debug
      ChainDB.CandidateContainsFutureBlocks {} -> Debug
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {} -> Error
    ChainDB.ChainSelectionForFutureBlock {} -> Debug
  getSeverityAnnotation (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis {} -> Info
    LedgerDB.ReplayFromSnapshot {} -> Info
    LedgerDB.ReplayedBlock {} -> Debug
  getSeverityAnnotation (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot {} -> Info
    LedgerDB.DeletedSnapshot {} -> Debug
    LedgerDB.InvalidSnapshot {} -> Error
  getSeverityAnnotation (ChainDB.TraceCopyToImmutableDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmutableDB {} -> Debug
    ChainDB.NoBlocksToCopyToImmutableDB -> Debug
  getSeverityAnnotation (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC {} -> Debug
    ChainDB.ScheduledGC {} -> Debug
  getSeverityAnnotation (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB {} -> Debug
    ChainDB.ClosedDB {} -> Debug
    ChainDB.OpenedImmutableDB {} -> Debug
    ChainDB.OpenedVolatileDB -> Debug
    ChainDB.OpenedLgrDB -> Debug
  getSeverityAnnotation (ChainDB.TraceFollowerEvent ev) = case ev of
    ChainDB.NewFollower {} -> Debug
    ChainDB.FollowerNoLongerInMem {} -> Debug
    ChainDB.FollowerSwitchToMem {} -> Debug
    ChainDB.FollowerNewImmIterator {} -> Debug
  getSeverityAnnotation (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation e -> case e of
      ChainDB.InvalidBlock {} -> Error
      ChainDB.InvalidCandidate {} -> Error
      ChainDB.ValidCandidate {} -> Info
      ChainDB.CandidateContainsFutureBlocks {} -> Info
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {} -> Error
  getSeverityAnnotation (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.UnknownRangeRequested _ -> Error
    ChainDB.StreamFromVolatileDB {} -> Debug
    ChainDB.StreamFromImmutableDB _ _ -> Debug
    ChainDB.StreamFromBoth {} -> Debug
    ChainDB.BlockMissingFromVolatileDB _ -> Debug
    ChainDB.BlockWasCopiedToImmutableDB _ -> Debug
    ChainDB.BlockGCedFromVolatileDB _ -> Debug
    ChainDB.SwitchBackToVolatileDB -> Debug
  getSeverityAnnotation (ChainDB.TraceImmutableDBEvent ev) = case ev of
    ImmutableDB.NoValidLastLocation -> Debug
    ImmutableDB.ValidatedLastLocation {} -> Debug
    ImmutableDB.ValidatingChunk _ -> Debug
    ImmutableDB.MissingChunkFile _ -> Error
    ImmutableDB.InvalidChunkFile {} -> Error
    ImmutableDB.ChunkFileDoesntFit {} -> Error
    ImmutableDB.MissingPrimaryIndex _ -> Debug
    ImmutableDB.MissingSecondaryIndex _ -> Debug
    ImmutableDB.InvalidPrimaryIndex _ -> Warning
    ImmutableDB.InvalidSecondaryIndex _ -> Warning
    ImmutableDB.RewritePrimaryIndex _ -> Debug
    ImmutableDB.RewriteSecondaryIndex _ -> Debug
    ImmutableDB.Migrating _ -> Info
    ImmutableDB.DeletingAfter _ -> Debug
    ImmutableDB.DBAlreadyClosed -> Debug
    ImmutableDB.DBClosed -> Debug
    ImmutableDB.TraceCacheEvent _ -> Debug
  getSeverityAnnotation (ChainDB.TraceVolatileDBEvent ev) = case ev of
    VolatileDB.DBAlreadyClosed -> Debug
    VolatileDB.DBAlreadyOpen -> Debug
    VolatileDB.BlockAlreadyHere _ -> Debug
    VolatileDB.TruncateCurrentFile _ -> Debug
    VolatileDB.Truncate {} -> Error
    VolatileDB.InvalidFileNames _ -> Warning

instance HasPrivacyAnnotation (TraceBlockFetchServerEvent blk)

instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk) where
  getSeverityAnnotation TraceBlockFetchServerSendBlock = Debug

instance HasPrivacyAnnotation (TraceChainSyncClientEvent blk)

instance HasSeverityAnnotation (TraceChainSyncClientEvent blk) where
  getSeverityAnnotation TraceDownloadedHeader {} = Debug
  getSeverityAnnotation TraceFoundIntersection {} = Debug
  getSeverityAnnotation TraceRolledBack {} = Debug
  getSeverityAnnotation TraceException {} = Error
  getSeverityAnnotation (TraceTermination result) = case result of
    ForkTooDeep {} -> Error
    NoMoreIntersection {} -> Error
    RolledBackPastIntersection {} -> Error
    AskedToTerminate -> Debug

instance HasTextFormatter (TraceChainSyncClientEvent blk)

instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk)

instance HasSeverityAnnotation (TraceChainSyncServerEvent blk) where
  getSeverityAnnotation TraceChainSyncServerRead {} = Debug
  getSeverityAnnotation TraceChainSyncServerReadBlocked {} = Debug
  getSeverityAnnotation TraceChainSyncRollForward {} = Debug
  getSeverityAnnotation TraceChainSyncRollBackward {} = Debug

instance HasSeverityAnnotation (Either FetchDecline [Point (Header blk)])

instance HasPrivacyAnnotation (Either FetchDecline [Point (Header blk)])

instance (StandardHash blk) => HasTextFormatter (TraceChainSyncServerEvent blk)

instance HasPrivacyAnnotation (TraceEventMempool blk)

instance HasSeverityAnnotation MorphoTransactionError where
  getSeverityAnnotation MorphoCandidateBeforeCheckpoint = Info
  getSeverityAnnotation MorphoAlreadyCheckpointed = Debug
  getSeverityAnnotation MorphoWrongDistance = Error
  getSeverityAnnotation MorphoInvalidSignature = Error
  getSeverityAnnotation MorphoDuplicateVote = Debug
  getSeverityAnnotation MorphoUnknownPublicKey = Notice

instance HasSeverityAnnotation (TraceEventMempool (MorphoBlock h c)) where
  getSeverityAnnotation TraceMempoolAddedTx {} = Info
  getSeverityAnnotation (TraceMempoolRejectedTx _ (_, reason) _) = getSeverityAnnotation reason
  getSeverityAnnotation (TraceMempoolRemoveTxs _ _) = Debug
  getSeverityAnnotation TraceMempoolManuallyRemovedTxs {} = Debug

instance HasPrivacyAnnotation ()

instance HasSeverityAnnotation () where
  getSeverityAnnotation () = Info

instance HasPrivacyAnnotation (TraceForgeEvent blk)

instance HasSeverityAnnotation (MorphoError (MorphoBlock h c)) where
  getSeverityAnnotation (MorphoTransactionError _ txErr) = getSeverityAnnotation txErr
  getSeverityAnnotation (MorphoInvalidHash _ _) = Error

instance HasSeverityAnnotation (InvalidBlockReason (MorphoBlock h c)) where
  getSeverityAnnotation (ChainDB.ValidationError (ExtValidationErrorLedger err)) = getSeverityAnnotation err
  getSeverityAnnotation (ChainDB.ValidationError ExtValidationErrorHeader {}) = Error
  getSeverityAnnotation (ChainDB.InFutureExceedsClockSkew _) = Warning

instance HasSeverityAnnotation (TraceForgeEvent (MorphoBlock h c)) where
  getSeverityAnnotation TraceForgedBlock {} = Info
  getSeverityAnnotation TraceStartLeadershipCheck {} = Debug
  getSeverityAnnotation TraceNodeNotLeader {} = Info
  getSeverityAnnotation TraceNodeCannotForge {} = Error
  getSeverityAnnotation TraceNodeIsLeader {} = Info
  getSeverityAnnotation TraceNoLedgerState {} = Error
  getSeverityAnnotation TraceNoLedgerView {} = Error
  getSeverityAnnotation TraceBlockFromFuture {} = Error
  getSeverityAnnotation TraceSlotIsImmutable {} = Error
  getSeverityAnnotation TraceAdoptedBlock {} = Info
  getSeverityAnnotation TraceDidntAdoptBlock {} = Error
  getSeverityAnnotation (TraceForgedInvalidBlock _ _ reason) = getSeverityAnnotation reason
  getSeverityAnnotation TraceBlockContext {} = Debug
  getSeverityAnnotation TraceLedgerState {} = Debug
  getSeverityAnnotation TraceLedgerView {} = Debug
  getSeverityAnnotation TraceForgeStateUpdateError {} = Error

instance HasPrivacyAnnotation (TraceLocalTxSubmissionServerEvent blk)

instance HasSeverityAnnotation (TraceLocalTxSubmissionServerEvent blk) where
  getSeverityAnnotation TraceReceivedTx {} = Info

instance HasTextFormatter (TraceLocalTxSubmissionServerEvent blk) where
  formatText _ = pack . show . toList

instance
  (LedgerSupportsProtocol blk) =>
  HasTextFormatter (ChainDB.TraceAddBlockEvent blk)
  where
  formatText _ = pack . show . toList

instance HasTextFormatter (TraceBlockFetchServerEvent blk) where
  formatText _ = pack . show . toList

condenseT :: Condense a => a -> Text
condenseT = pack . condense

showT :: Show a => a -> Text
showT = pack . show

instance (Signable (BftDSIGN c) (MorphoStdHeader h c), MorphoStateDefaultConstraints h c) => HasTextFormatter (TraceLabelCreds (TraceForgeEvent (MorphoBlock h c))) where
  formatText (TraceLabelCreds _ t) = formatText t

instance (Signable (BftDSIGN c) (MorphoStdHeader h c), MorphoStateDefaultConstraints h c) => HasTextFormatter (TraceForgeEvent (MorphoBlock h c)) where
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
    TraceNodeCannotForge slotNo reason ->
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
    TraceBlockContext slotNo _blk point ->
      const $
        "The block to connect to is at slot "
          <> showT (unSlotNo slotNo)
          <> ", with point"
          <> showT point
    TraceLedgerState slotNo point ->
      const $
        "The ledger state at the block to connect to is at slot "
          <> showT (unSlotNo slotNo)
          <> ", with point"
          <> showT point
    TraceLedgerView slotNo ->
      const $
        "Ledger view for the current slot number "
          <> showT (unSlotNo slotNo)
          <> " obtained"
    TraceForgeStateUpdateError slotNo err ->
      const $
        "Updating the forge state at slot "
          <> showT (unSlotNo slotNo)
          <> " failed with "
          <> showT err

instance (Signable (BftDSIGN c) (MorphoStdHeader h c), MorphoStateDefaultConstraints h c) => HasTextFormatter (ChainDB.TraceEvent (MorphoBlock h c)) where
  formatText = \case
    ChainDB.TraceAddBlockEvent ev -> case ev of
      ChainDB.IgnoreBlockOlderThanK pt ->
        const $
          "Ignoring block older than K: " <> condenseT pt
      ChainDB.IgnoreBlockAlreadyInVolatileDB pt -> \_o ->
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
      ChainDB.AddedToCurrentChain _ _ _ c -> \_o ->
        "Chain extended, new tip: " <> condenseT (AF.headPoint c)
      ChainDB.SwitchedToAFork _ _ _ c -> \_o ->
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
      ChainDB.AddedBlockToVolatileDB pt _ _ -> \_o ->
        "Chain added block " <> condenseT pt
      ChainDB.ChainSelectionForFutureBlock pt -> \_o ->
        "Chain selection run for block previously from future: " <> condenseT pt
    ChainDB.TraceLedgerReplayEvent ev -> case ev of
      LedgerDB.ReplayFromGenesis _replayTo ->
        const
          "Replaying ledger from genesis"
      LedgerDB.ReplayFromSnapshot snap tip' _replayTo -> \_o ->
        "Replaying ledger from snapshot " <> showT snap <> " at "
          <> condenseT tip'
      LedgerDB.ReplayedBlock pt _ replayTo -> \_o ->
        "Replayed block: slot " <> showT (realPointSlot pt) <> " of " <> showT (pointSlot replayTo)
    ChainDB.TraceLedgerEvent ev -> case ev of
      LedgerDB.TookSnapshot snap pt -> \_o ->
        "Took ledger snapshot " <> showT snap <> " at " <> condenseT pt
      LedgerDB.DeletedSnapshot snap -> \_o ->
        "Deleted old snapshot " <> showT snap
      LedgerDB.InvalidSnapshot snap failure -> \_o ->
        "Invalid snapshot " <> showT snap <> showT failure
    ChainDB.TraceCopyToImmutableDBEvent ev -> case ev of
      ChainDB.CopiedBlockToImmutableDB pt -> \_o ->
        "Copied block " <> condenseT pt <> " to the ImmutableDB"
      ChainDB.NoBlocksToCopyToImmutableDB ->
        const
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
      ChainDB.OpenedImmutableDB immTip epoch -> \_o ->
        "Opened imm db with immutable tip at " <> condenseT immTip
          <> " and epoch "
          <> showT epoch
      ChainDB.OpenedVolatileDB -> const "Opened vol db"
      ChainDB.OpenedLgrDB -> const "Opened lgr db"
    ChainDB.TraceFollowerEvent ev -> case ev of
      ChainDB.NewFollower -> const "New follower was created"
      ChainDB.FollowerNoLongerInMem _ -> const "FollowerNoLongerInMem"
      ChainDB.FollowerSwitchToMem _ _ -> const "FollowerSwitchToMem"
      ChainDB.FollowerNewImmIterator _ _ -> const "FollowerNewImmIterator"
    ChainDB.TraceInitChainSelEvent ev -> case ev of
      ChainDB.InitChainSelValidation _ -> const "InitChainSelValidation"
    ChainDB.TraceIteratorEvent ev -> case ev of
      ChainDB.UnknownRangeRequested _ -> const "UnknownRangeRequested"
      ChainDB.BlockMissingFromVolatileDB _ -> const "BlockMissingFromVolDB"
      ChainDB.StreamFromImmutableDB _ _ -> const "StreamFromImmDB"
      ChainDB.StreamFromBoth {} -> const "StreamFromBoth"
      ChainDB.StreamFromVolatileDB {} -> const "StreamFromVolDB"
      ChainDB.BlockWasCopiedToImmutableDB _ -> const "BlockWasCopiedToImmDB"
      ChainDB.BlockGCedFromVolatileDB _ -> const "BlockGCedFromVolDB"
      ChainDB.SwitchBackToVolatileDB -> const "SwitchBackToVolDB"
    ChainDB.TraceImmutableDBEvent ev -> \_o -> Text.append "TraceImmDBEvent" (showT ev)
    ChainDB.TraceVolatileDBEvent ev -> \_o -> Text.append "TraceVolDBEvent " (showT ev)

instance-- (Show (GenTxId blk), Show (ApplyTxErr blk), Show (GenTx blk)) =>
  HasTextFormatter (TraceEventMempool blk)

instance HasTextFormatter () where
  formatText _ = pack . show . toList

-- | Tracing wrapper which includes current tip in the logs (thus it requires
-- it from the context).
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

instance HasPrivacyAnnotation NtC.HandshakeTr

instance HasSeverityAnnotation NtC.HandshakeTr where
  getSeverityAnnotation (WithMuxBearer _ ev) = getSeverityAnnotation ev

instance HasPrivacyAnnotation NtN.HandshakeTr

instance HasSeverityAnnotation NtN.HandshakeTr where
  getSeverityAnnotation (WithMuxBearer _ ev) = getSeverityAnnotation ev

instance HasPrivacyAnnotation NtN.AcceptConnectionsPolicyTrace

instance HasSeverityAnnotation NtN.AcceptConnectionsPolicyTrace where
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionRateLimiting {} = Notice
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionHardLimit {} = Warning

instance HasPrivacyAnnotation DiffusionInitializationTracer

instance HasSeverityAnnotation DiffusionInitializationTracer where
  getSeverityAnnotation RunServer {} = Debug
  getSeverityAnnotation RunLocalServer {} = Debug
  getSeverityAnnotation UsingSystemdSocket {} = Debug
  getSeverityAnnotation CreateSystemdSocketForSnocketPath {} = Debug
  getSeverityAnnotation CreatedLocalSocket {} = Debug
  getSeverityAnnotation ConfiguringLocalSocket {} = Debug
  getSeverityAnnotation ListeningLocalSocket {} = Debug
  getSeverityAnnotation LocalSocketUp {} = Debug
  getSeverityAnnotation CreatingServerSocket {} = Debug
  getSeverityAnnotation ConfiguringServerSocket {} = Debug
  getSeverityAnnotation ListeningServerSocket {} = Debug
  getSeverityAnnotation ServerSocketUp {} = Debug
  getSeverityAnnotation UnsupportedLocalSystemdSocket {} = Error
  getSeverityAnnotation UnsupportedReadySocketCase {} = Error
  getSeverityAnnotation DiffusionErrored {} = Error

instance HasPrivacyAnnotation TraceLedgerPeers

instance HasSeverityAnnotation TraceLedgerPeers where
  getSeverityAnnotation PickedPeer {} = Info
  getSeverityAnnotation PickedPeers {} = Info
  getSeverityAnnotation FetchingNewLedgerState {} = Info

instance HasPrivacyAnnotation (TraceFetchClientState header)

instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation AddedFetchRequest {} = Debug
  getSeverityAnnotation AcknowledgedFetchRequest {} = Debug
  getSeverityAnnotation StartedFetchBatch {} = Debug
  getSeverityAnnotation CompletedBlockFetch {} = Debug
  getSeverityAnnotation CompletedFetchBatch {} = Debug
  getSeverityAnnotation RejectedFetchBatch {} = Error
  getSeverityAnnotation ClientTerminating {} = Debug

instance HasPrivacyAnnotation (TraceSendRecv a)

instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation TraceSendMsg {} = Debug
  getSeverityAnnotation TraceRecvMsg {} = Debug

instance HasPrivacyAnnotation a => HasPrivacyAnnotation (TraceLabelPeer peer a)

instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelPeer peer a) where
  getSeverityAnnotation (TraceLabelPeer _ a) = getSeverityAnnotation a

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
          Left FetchDeclineConcurrencyLimit {} -> Debug
          Right _ -> Debug

instance HasPrivacyAnnotation (TraceTxSubmissionInbound txid tx)

instance HasSeverityAnnotation (TraceTxSubmissionInbound txid tx) where
  getSeverityAnnotation TraceTxSubmissionCollected {} = Info
  getSeverityAnnotation TraceTxSubmissionProcessed {} = Info
  getSeverityAnnotation TraceTxInboundTerminated {} = Debug
  getSeverityAnnotation TraceTxInboundCanRequestMoreTxs {} = Debug
  getSeverityAnnotation TraceTxInboundCannotRequestMoreTxs {} = Debug

instance HasPrivacyAnnotation (TraceTxSubmissionOutbound txid tx)

instance HasSeverityAnnotation (TraceTxSubmissionOutbound txid tx) where
  getSeverityAnnotation TraceTxSubmissionOutboundRecvMsgRequestTxs {} = Info
  getSeverityAnnotation TraceTxSubmissionOutboundSendMsgReplyTxs {} = Info
  getSeverityAnnotation TraceControlMessage {} = Debug

instance HasPrivacyAnnotation (WithAddr addr ErrorPolicyTrace)

instance HasSeverityAnnotation ErrorPolicyTrace where
  getSeverityAnnotation ErrorPolicySuspendPeer {} = Warning -- peer misbehaved
  getSeverityAnnotation ErrorPolicySuspendConsumer {} = Notice -- peer temporarily not useful
  getSeverityAnnotation ErrorPolicyLocalNodeError {} = Error
  getSeverityAnnotation ErrorPolicyResumePeer {} = Debug
  getSeverityAnnotation ErrorPolicyKeepSuspended {} = Debug
  getSeverityAnnotation ErrorPolicyResumeConsumer {} = Debug
  getSeverityAnnotation ErrorPolicyResumeProducer {} = Debug
  getSeverityAnnotation ErrorPolicyUnhandledApplicationException {} = Error
  getSeverityAnnotation ErrorPolicyUnhandledConnectionException {} = Error
  getSeverityAnnotation ErrorPolicyAcceptException {} = Error

instance HasSeverityAnnotation (WithAddr addr ErrorPolicyTrace) where
  getSeverityAnnotation (WithAddr _ ev) = getSeverityAnnotation ev

instance HasPrivacyAnnotation (WithDomainName DnsTrace)

instance HasSeverityAnnotation DnsTrace where
  getSeverityAnnotation DnsTraceLookupException {} = Error
  getSeverityAnnotation DnsTraceLookupAError {} = Error
  getSeverityAnnotation DnsTraceLookupAAAAError {} = Error
  getSeverityAnnotation DnsTraceLookupSRVError {} = Error
  getSeverityAnnotation DnsTraceLookupIPv6First = Debug
  getSeverityAnnotation DnsTraceLookupIPv4First = Debug
  getSeverityAnnotation DnsTraceLookupAResult {} = Debug
  getSeverityAnnotation DnsTraceLookupAAAAResult {} = Debug
  getSeverityAnnotation DnsTraceLookupSRVResult {} = Debug

instance HasSeverityAnnotation a => HasSeverityAnnotation (WithDomainName a) where
  getSeverityAnnotation (WithDomainName _ ev) = getSeverityAnnotation ev

instance HasPrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))

instance HasSeverityAnnotation (SubscriptionTrace a) where
  getSeverityAnnotation SubscriptionTraceConnectStart {} = Debug
  getSeverityAnnotation (SubscriptionTraceConnectEnd _ ConnectSuccess) = Debug
  getSeverityAnnotation (SubscriptionTraceConnectEnd _ ConnectSuccessLast) = Debug
  getSeverityAnnotation (SubscriptionTraceConnectEnd _ ConnectValencyExceeded) = Notice
  getSeverityAnnotation SubscriptionTraceConnectException {} = Error
  getSeverityAnnotation SubscriptionTraceSocketAllocationException {} = Error
  getSeverityAnnotation SubscriptionTraceTryConnectToPeer {} = Debug
  getSeverityAnnotation SubscriptionTraceSkippingPeer {} = Debug
  getSeverityAnnotation SubscriptionTraceSubscriptionRunning = Info
  getSeverityAnnotation SubscriptionTraceSubscriptionWaiting {} = Debug
  getSeverityAnnotation SubscriptionTraceSubscriptionFailed = Warning
  getSeverityAnnotation SubscriptionTraceSubscriptionWaitingNewConnection {} = Debug
  getSeverityAnnotation SubscriptionTraceStart {} = Info
  getSeverityAnnotation SubscriptionTraceRestart {} = Debug
  getSeverityAnnotation SubscriptionTraceConnectionExist {} = Debug
  getSeverityAnnotation SubscriptionTraceUnsupportedRemoteAddr {} = Error
  getSeverityAnnotation SubscriptionTraceMissingLocalAddress = Error
  getSeverityAnnotation SubscriptionTraceApplicationException {} = Error
  getSeverityAnnotation SubscriptionTraceAllocateSocket {} = Debug
  getSeverityAnnotation SubscriptionTraceCloseSocket {} = Debug

instance HasPrivacyAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))

instance HasSeverityAnnotation a => HasSeverityAnnotation (WithIPList a) where
  getSeverityAnnotation (WithIPList _ _ a) = getSeverityAnnotation a

instance HasPrivacyAnnotation (Identity (SubscriptionTrace LocalAddress))

instance HasSeverityAnnotation a => HasSeverityAnnotation (Identity a) where
  getSeverityAnnotation (Identity ev) = getSeverityAnnotation ev

instance HasPrivacyAnnotation (WithMuxBearer peer MuxTrace)

instance HasSeverityAnnotation (WithMuxBearer peer MuxTrace) where
  getSeverityAnnotation (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart -> Debug
    MuxTraceRecvHeaderEnd {} -> Debug
    MuxTraceRecvStart {} -> Debug
    MuxTraceRecvEnd {} -> Debug
    MuxTraceSendStart {} -> Debug
    MuxTraceSendEnd -> Debug
    MuxTraceState {} -> Debug
    MuxTraceCleanExit {} -> Debug
    MuxTraceExceptionExit {} -> Notice
    MuxTraceChannelRecvStart {} -> Debug
    MuxTraceChannelRecvEnd {} -> Debug
    MuxTraceChannelSendStart {} -> Debug
    MuxTraceChannelSendEnd {} -> Debug
    MuxTraceHandshakeStart -> Debug
    MuxTraceHandshakeClientEnd {} -> Debug
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
    MuxTraceTerminating _ _ -> Debug

instance HasTextFormatter NtN.HandshakeTr where
  formatText a _ = showT a

instance HasTextFormatter NtC.HandshakeTr where
  formatText a _ = showT a

instance HasTextFormatter NtN.AcceptConnectionsPolicyTrace where
  formatText a _ = showT a

instance HasTextFormatter DiffusionInitializationTracer

instance HasTextFormatter TraceLedgerPeers

instance HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText _ = pack . show . toList

instance HasTextFormatter (TraceLabelPeer peer a) where
  formatText _ = pack . show . toList

instance HasTextFormatter (TraceTxSubmissionInbound txid tx) where
  formatText _ = pack . show . toList

instance
  (Show tx, Show txid) =>
  HasTextFormatter (TraceTxSubmissionOutbound txid tx)
  where
  formatText a _ = showT a

instance HasTextFormatter (WithAddr addr ErrorPolicyTrace) where
  formatText _ = pack . show . toList

instance HasTextFormatter (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  formatText _ = pack . show . toList

instance HasTextFormatter (WithDomainName DnsTrace) where
  formatText _ = pack . show . toList

instance HasTextFormatter (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  formatText _ = pack . show . toList

instance
  (Show peer) =>
  HasTextFormatter (WithMuxBearer peer MuxTrace)
  where
  formatText (WithMuxBearer peer ev) = \_o ->
    "Bearer on " <> pack (show peer)
      <> " event: "
      <> pack (show ev)
