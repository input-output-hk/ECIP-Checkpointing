{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Tracing.TracingOrphanInstances where

import Cardano.BM.Tracing
  ( HasSeverityAnnotation (..),
    Severity (..),
    TracingVerbosity (..),
  )
import Cardano.Prelude hiding (show)
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Tracing.Types
import Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket (SockAddr)
import Ouroboros.Consensus.Block
  ( ForgeState (..),
  )
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
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
import Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import Ouroboros.Consensus.Util.Condense (Condense, condense)
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState
  ( TraceFetchClientState (..),
    TraceLabelPeer (..),
  )
import Ouroboros.Network.BlockFetch.Decision
  ( FetchDecision,
  )
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
  ( ErrorPolicyTrace (..),
    TraceSendRecv (..),
    WithAddr (..),
  )
import qualified Ouroboros.Network.NodeToNode as NtN
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
import Prelude (String, id)

instance HasSeverityAnnotation (ExtractStateTrace h c) where
  getSeverityAnnotation MorphoStateTrace {} = Info
  getSeverityAnnotation PushingCheckpoint {} = Debug
  getSeverityAnnotation ExtractTxErrorTrace {} = Error
  getSeverityAnnotation WontPushCheckpointTrace {} = Info

instance HasSeverityAnnotation TraceBlockchainTimeEvent where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation PoWNodeRpcTrace where
  getSeverityAnnotation RpcPushedCheckpoint {} = Notice
  getSeverityAnnotation RpcLatestPoWBlock {} = Notice
  getSeverityAnnotation RpcNetworkError {} = Error
  getSeverityAnnotation RpcResponseParseError {} = Error

instance HasSeverityAnnotation (TimeTravelError blk) where
  getSeverityAnnotation _ = Error

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

instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation (TraceChainSyncClientEvent blk) where
  getSeverityAnnotation (TraceDownloadedHeader _) = Info
  getSeverityAnnotation TraceFoundIntersection {} = Info
  getSeverityAnnotation (TraceRolledBack _) = Notice
  getSeverityAnnotation (TraceException _) = Warning

instance HasSeverityAnnotation (TraceChainSyncServerEvent blk) where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation (TraceEventMempool blk) where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation (ForgeState c) where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation () where
  getSeverityAnnotation () = Info

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

instance HasSeverityAnnotation (TraceLocalTxSubmissionServerEvent blk) where
  getSeverityAnnotation _ = Info

data WithTip blk a = WithTip (Point blk) a

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

instance HasSeverityAnnotation NtC.HandshakeTr where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation NtN.HandshakeTr where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation NtN.AcceptConnectionsPolicyTrace where
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionHardLimit {} = Warning

instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation _ = Debug

instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelPeer peer a)

instance HasSeverityAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]

instance HasSeverityAnnotation (TraceTxSubmissionInbound txid tx) where
  getSeverityAnnotation _ = Info

instance HasSeverityAnnotation (TraceTxSubmissionOutbound txid tx) where
  getSeverityAnnotation _ = Info

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

instance HasSeverityAnnotation (WithDomainName DnsTrace)

instance HasSeverityAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))

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
