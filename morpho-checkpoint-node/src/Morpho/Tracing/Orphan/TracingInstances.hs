{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Tracing.Orphan.TracingInstances where

import Cardano.BM.Tracing
  ( HasPrivacyAnnotation (..),
    HasSeverityAnnotation (..),
    Severity (..),
  )
import Cardano.Prelude hiding (show)
import Morpho.Ledger.Block
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.State
import Morpho.RPC.Abstract
import Morpho.Tracing.Orphan.MorphoToJSONInstances ()
import Morpho.Tracing.Types
import Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket (SockAddr)
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Ledger.Extended
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
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Types as VolatileDB
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

instance HasPrivacyAnnotation (TraceBlockchainTimeEvent t)

instance HasSeverityAnnotation (TraceBlockchainTimeEvent t) where
  getSeverityAnnotation (TraceStartTimeInTheFuture _ _) = Debug
  getSeverityAnnotation (TraceCurrentSlotUnknown _ _) = Debug
  getSeverityAnnotation (TraceSystemClockMovedBack _ _) = Warning

instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelCreds a) where
  getSeverityAnnotation (TraceLabelCreds _ a) = getSeverityAnnotation a

instance HasPrivacyAnnotation (TraceLabelCreds a)

instance HasPrivacyAnnotation (ExtractStateTrace h c)

instance HasSeverityAnnotation (ExtractStateTrace h c) where
  getSeverityAnnotation MorphoStateTrace {} = Info
  getSeverityAnnotation WontPushCheckpointTrace {} = Debug
  getSeverityAnnotation VoteErrorTrace {} = Error

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

instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk)

instance HasSeverityAnnotation (TraceChainSyncServerEvent blk) where
  getSeverityAnnotation TraceChainSyncServerRead {} = Debug
  getSeverityAnnotation TraceChainSyncServerReadBlocked {} = Debug
  getSeverityAnnotation TraceChainSyncRollForward {} = Debug
  getSeverityAnnotation TraceChainSyncRollBackward {} = Debug

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
