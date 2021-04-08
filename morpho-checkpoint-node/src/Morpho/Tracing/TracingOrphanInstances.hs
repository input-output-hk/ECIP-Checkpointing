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
    contramap,
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
-- We do need some consensus imports to provide useful trace messages for some
-- network protocols

import Cardano.Crypto.DSIGN
import Cardano.Prelude hiding (show)
import Data.Aeson (ToJSON (..))
import Data.Text (pack)
import qualified Data.Text as Text
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
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
  ( BlockSupportsProtocol,
    CannotForge,
    ForgeStateUpdateError,
    RealPoint,
    headerPoint,
    realPointSlot,
  )
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId, HasTxId, txId)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol,
  )
import Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
  ( TraceBlockFetchServerEvent,
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( InvalidBlockReason,
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
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
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

instance HasSeverityAnnotation (TraceBlockchainTimeEvent t) where
  getSeverityAnnotation (TraceStartTimeInTheFuture _ _) = Debug
  getSeverityAnnotation (TraceCurrentSlotUnknown _ _) = Debug
  getSeverityAnnotation (TraceSystemClockMovedBack _ _) = Warning

instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelCreds a) where
  getSeverityAnnotation (TraceLabelCreds _ a) = getSeverityAnnotation a

instance HasPrivacyAnnotation (TraceLabelCreds a)

instance ToObject a => ToObject (TraceLabelCreds a) where
  toObject _verb (TraceLabelCreds _ a) = toObject _verb a

instance (ToObject a, HasTextFormatter a, Transformable Text IO a) => Transformable Text IO (TraceLabelCreds a) where
  trTransformer _verb tr = contramap (\(TraceLabelCreds _ a) -> a) (trStructuredText _verb tr)

instance (HashAlgorithm h, BftCrypto c) => Transformable Text IO (ExtractStateTrace h c) where
  trTransformer = trStructuredText

instance (HashAlgorithm h, BftCrypto c) => ToObject (ExtractStateTrace h c)

instance (HashAlgorithm h, BftCrypto c) => HasTextFormatter (ExtractStateTrace h c) where
  formatText (MorphoStateTrace st) _ = pack $ "Current Ledger State: " ++ show st
  formatText (WontPushCheckpointTrace reason) _ =
    pack $
      "Checkpoint doesn't need to be pushed: " ++ show reason
  formatText (VoteErrorTrace err) _ =
    pack $ "Error while trying to create a vote: " ++ show err

instance HasPrivacyAnnotation (ExtractStateTrace h c)

instance HasSeverityAnnotation (ExtractStateTrace h c) where
  getSeverityAnnotation MorphoStateTrace {} = Info
  getSeverityAnnotation WontPushCheckpointTrace {} = Info
  getSeverityAnnotation VoteErrorTrace {} = Error

instance BftCrypto c => ToObject (Header (MorphoBlock h c))

instance (HasSeverityAnnotation e, ToJSON e, Show e) => Transformable Text IO (RpcTrace e i o) where
  trTransformer = trStructuredText

instance Show e => HasTextFormatter (RpcTrace e i o) where
  formatText tr _ = pack $ show tr

instance ToJSON e => ToObject (RpcTrace e i o)

instance HasPrivacyAnnotation (RpcTrace e i o)

instance HasSeverityAnnotation e => HasSeverityAnnotation (RpcTrace e i o) where
  getSeverityAnnotation (RpcTrace _ _ RpcStart) = Info
  getSeverityAnnotation (RpcTrace GetLatestBlock _ (RpcSuccess Nothing)) = Notice
  getSeverityAnnotation (RpcTrace _ _ (RpcSuccess _)) = Info
  getSeverityAnnotation (RpcTrace _ _ (RpcEvent x)) = getSeverityAnnotation x

instance (HashAlgorithm h, BftCrypto c) => Transformable Text IO (TimeTravelError (MorphoBlock h c)) where
  trTransformer = trStructured

instance HasPrivacyAnnotation (TimeTravelError blk)

instance HasSeverityAnnotation (TimeTravelError blk) where
  getSeverityAnnotation (ChainNotLongEnough _ _) = Info
  getSeverityAnnotation (LedgerStateNotFoundAt _) = Error

instance (HashAlgorithm h, BftCrypto c) => ToObject (TimeTravelError (MorphoBlock h c))

instance (BftCrypto c, HashAlgorithm h) => ToObject (GenTx (MorphoBlock h c))

instance ToObject MorphoTransactionError

instance ToObject (Vote, MorphoTransactionError)

instance ToObject (MorphoError (MorphoBlock h c))

--

-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@

--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation (ChainDB.TraceEvent blk)

instance HasSeverityAnnotation (ChainDB.TraceEvent blk) where
  getSeverityAnnotation (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK {} -> Info
    ChainDB.IgnoreBlockAlreadyInVolatileDB {} -> Info
    ChainDB.IgnoreInvalidBlock {} -> Info
    ChainDB.AddedBlockToQueue {} -> Debug
    ChainDB.BlockInTheFuture {} -> Info
    ChainDB.AddedBlockToVolatileDB {} -> Debug
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
  getSeverityAnnotation (ChainDB.TraceCopyToImmutableDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmutableDB {} -> Debug
    ChainDB.NoBlocksToCopyToImmutableDB -> Debug
  getSeverityAnnotation (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC {} -> Debug
    ChainDB.ScheduledGC {} -> Debug
  getSeverityAnnotation (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB {} -> Info
    ChainDB.ClosedDB {} -> Info
    ChainDB.OpenedImmutableDB {} -> Info
    ChainDB.OpenedVolatileDB -> Info
    ChainDB.OpenedLgrDB -> Info
  getSeverityAnnotation (ChainDB.TraceFollowerEvent ev) = case ev of
    ChainDB.NewFollower {} -> Debug
    ChainDB.FollowerNoLongerInMem {} -> Debug
    ChainDB.FollowerSwitchToMem {} -> Debug
    ChainDB.FollowerNewImmIterator {} -> Debug
  getSeverityAnnotation (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation {} -> Debug
  getSeverityAnnotation (ChainDB.TraceIteratorEvent _) = Debug
  getSeverityAnnotation (ChainDB.TraceImmutableDBEvent _) = Debug
  getSeverityAnnotation (ChainDB.TraceVolatileDBEvent _) = Debug

instance HasPrivacyAnnotation (TraceBlockFetchServerEvent blk)

instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TraceChainSyncClientEvent blk)

instance HasSeverityAnnotation (TraceChainSyncClientEvent blk) where
  getSeverityAnnotation TraceDownloadedHeader {} = Info
  getSeverityAnnotation TraceFoundIntersection {} = Info
  getSeverityAnnotation TraceRolledBack {} = Notice
  getSeverityAnnotation TraceException {} = Warning
  getSeverityAnnotation TraceTermination {} = Info

instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk)

instance HasSeverityAnnotation (TraceChainSyncServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation (Either FetchDecline [Point (Header blk)])

instance HasPrivacyAnnotation (Either FetchDecline [Point (Header blk)])

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
  getSeverityAnnotation TraceStartLeadershipCheck {} = Info
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
  getSeverityAnnotation TraceBlockContext {} = Info
  getSeverityAnnotation TraceLedgerState {} = Info
  getSeverityAnnotation TraceLedgerView {} = Info
  getSeverityAnnotation TraceForgeStateUpdateError {} = Error

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

instance (BftCrypto c, HashAlgorithm h) => Transformable Text IO (TraceChainSyncClientEvent (MorphoBlock h c)) where
  trTransformer = trStructured

instance (BftCrypto c, HashAlgorithm h) => Transformable Text IO (TraceChainSyncServerEvent (MorphoBlock h c)) where
  trTransformer = trStructured

instance
  ( Show (GenTx blk),
    Show (ApplyTxErr blk),
    blk ~ MorphoBlock h c
  ) =>
  Transformable Text IO (TraceEventMempool blk)
  where
  trTransformer = trStructuredText

condenseT :: Condense a => a -> Text
condenseT = pack . condense

showT :: Show a => a -> Text
showT = pack . show

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  Transformable Text IO (TraceForgeEvent (MorphoBlock h c))
  where
  trTransformer = trStructuredText

instance
  ( tx ~ GenTx blk,
    Condense (HeaderHash blk),
    HasTxId tx,
    LedgerSupportsProtocol blk,
    Show (TxId tx),
    Show blk,
    Show (CannotForge blk),
    Show (ForgeStateUpdateError blk)
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

instance Transformable Text IO (TraceLocalTxSubmissionServerEvent (MorphoBlock h c)) where
  trTransformer = trStructured

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  Transformable Text IO (ChainDB.TraceEvent (MorphoBlock h c))
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

--

-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.
instance ToObject BftValidationErr

instance ToObject LedgerDB.DiskSnapshot

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  ToObject (ExtValidationError (MorphoBlock h c))

instance ToObject (HeaderEnvelopeError (MorphoBlock h c))

instance (BlockSupportsProtocol blk, ValidateEnvelope blk) => ToObject (HeaderError blk)

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  ToObject (ChainDB.InvalidBlockReason (MorphoBlock h c))

instance ToObject (RealPoint (MorphoBlock h c))

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  ToObject (ChainDB.TraceEvent (MorphoBlock h c))

instance ToObject (TraceBlockFetchServerEvent blk)

instance (BftCrypto c, HashAlgorithm h) => ToObject (TraceChainSyncClientEvent (MorphoBlock h c))

instance (BftCrypto c, HashAlgorithm h) => ToObject (TraceChainSyncServerEvent (MorphoBlock h c))

instance ToObject (TraceEventMempool (MorphoBlock h c))

instance
  (Show (GenTxId blk), Show (ApplyTxErr blk), Show (GenTx blk)) =>
  HasTextFormatter (TraceEventMempool blk)
  where
  formatText a _ = pack $ show a

instance ToObject MempoolSize

instance HasTextFormatter () where
  formatText _ = pack . show . toList

-- ForgeState default value = ()
instance Transformable Text IO () where
  trTransformer = trStructuredText

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  ToObject (TraceForgeEvent (MorphoBlock h c))

instance ToObject (TraceLocalTxSubmissionServerEvent (MorphoBlock h c))

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

instance HasPrivacyAnnotation DiffusionInitializationTracer

instance HasSeverityAnnotation DiffusionInitializationTracer where
  getSeverityAnnotation (UnsupportedLocalSystemdSocket _) = Error
  getSeverityAnnotation UnsupportedReadySocketCase = Error
  getSeverityAnnotation (DiffusionErrored _) = Error
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation TraceLedgerPeers

instance HasSeverityAnnotation TraceLedgerPeers where
  getSeverityAnnotation _ = Info

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
    DnsTraceLookupSRVError {} -> Error
    DnsTraceLookupIPv6First -> Debug
    DnsTraceLookupIPv4First -> Debug
    DnsTraceLookupAResult {} -> Debug
    DnsTraceLookupAAAAResult {} -> Debug
    DnsTraceLookupSRVResult {} -> Debug

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
    MuxTraceTerminating _ _ -> Debug

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

instance Transformable Text IO DiffusionInitializationTracer where
  trTransformer = trStructuredText

instance HasTextFormatter DiffusionInitializationTracer

instance Transformable Text IO TraceLedgerPeers where
  trTransformer = trStructuredText

instance HasTextFormatter TraceLedgerPeers

instance
  (ToJSON peer, ToJSON (Point header)) =>
  Transformable Text IO [TraceLabelPeer peer (FetchDecision [Point header])]
  where
  trTransformer = trStructuredText

instance HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText _ = pack . show . toList

instance
  (ToJSON peer, ToJSON a, HasPrivacyAnnotation a, HasSeverityAnnotation a) =>
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
  (ToJSON tx, Show tx, ToJSON txid, Show txid) =>
  Transformable Text IO (TraceTxSubmissionOutbound txid tx)
  where
  trTransformer = trStructuredText

instance
  (Show tx, Show txid) =>
  HasTextFormatter (TraceTxSubmissionOutbound txid tx)
  where
  formatText a _ = showT a

instance ToJSON addr => Transformable Text IO (WithAddr addr ErrorPolicyTrace) where
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
  (Show peer, ToJSON peer) =>
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

instance ToJSON (Point header) => ToObject (FetchDecision [Point header])

instance ToObject NtC.HandshakeTr

instance ToObject NtN.HandshakeTr

instance ToObject NtN.AcceptConnectionsPolicyTrace

instance ToObject DiffusionInitializationTracer

instance ToObject TraceLedgerPeers

instance (BftCrypto c, HashAlgorithm h) => ToObject (Point (MorphoBlock h c))

instance ToObject SlotNo

instance (BftCrypto c, HashAlgorithm h) => ToObject (TraceFetchClientState (Header (MorphoBlock h c)))

instance (ToJSON peer, ToJSON (Point header)) => ToObject [TraceLabelPeer peer (FetchDecision [Point header])]

instance (ToJSON peer, ToJSON a) => ToObject (TraceLabelPeer peer a)

instance ToObject (TraceTxSubmissionInbound txid tx)

instance (ToJSON txid, ToJSON tx) => ToObject (TraceTxSubmissionOutbound txid tx)

instance ToJSON addr => ToObject (WithAddr addr ErrorPolicyTrace)

instance ToObject (WithIPList (SubscriptionTrace Socket.SockAddr))

instance ToObject (WithDomainName DnsTrace)

instance ToObject (WithDomainName (SubscriptionTrace Socket.SockAddr))

instance ToJSON peer => ToObject (WithMuxBearer peer MuxTrace)
