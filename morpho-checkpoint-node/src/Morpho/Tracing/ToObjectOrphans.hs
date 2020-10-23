{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Tracing.ToObjectOrphans
  ( WithTip (..),
    showTip,
    showWithTip,
  )
where

import Cardano.BM.Data.LogItem
import Cardano.BM.Data.Tracer
import Cardano.Prelude hiding (atomically, show)
import Data.Aeson
import Data.Text (pack)
import Morpho.Ledger.Block
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Morpho.Tracing.Types
import Network.Mux.Trace hiding (TraceLabelPeer)
import qualified Network.Socket as Socket (SockAddr)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
import Ouroboros.Consensus.Node.Tracers
import Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.Serialisation
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState
import Ouroboros.Network.BlockFetch.Decision
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Point
import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
import Ouroboros.Network.Protocol.TxSubmission.Type
import Ouroboros.Network.TxSubmission.Inbound
import Ouroboros.Network.TxSubmission.Outbound
import Prelude (String, id, show)

-- | Tracing wrapper which includes current tip in the logs (thus it requires
-- it from the context).
--
-- TODO: this should be moved to `ouroboros-consensus`.  Running in a seprate
-- STM transaction we risk reporting  wrong tip.
data WithTip blk a = WithTip (Point blk) a

showWithTip ::
  Condense (HeaderHash blk) =>
  (a -> String) ->
  WithTip blk a ->
  String
showWithTip customShow (WithTip tip a) = "[" ++ showTip MinimalVerbosity tip ++ "] " ++ customShow a

showTip ::
  Condense (HeaderHash blk) =>
  TracingVerbosity ->
  Point blk ->
  String
showTip verb tip =
  case pointHash tip of
    GenesisHash -> "genesis"
    BlockHash h -> trim $ condense h
    ++ case pointSlot tip of
      Origin -> "(origin)"
      At slot -> "@" ++ condense slot
  where
    trim :: [a] -> [a]
    trim = case verb of
      MinimalVerbosity -> take 7
      NormalVerbosity -> take 7
      MaximalVerbosity -> id

instance
  ( Show a,
    Condense (HeaderHash blk)
  ) =>
  Show (WithTip blk a)
  where
  show = showWithTip show

-- | instances of @Transformable@
-- transform @ChainSyncClient@
instance
  (Condense (HeaderHash blk), HasHeader (Header blk)) =>
  Transformable Text IO (TraceChainSyncClientEvent blk)
  where
  trTransformer verb tr = trStructured verb tr

-- transform @ChainSyncServer@
instance Condense (HeaderHash blk) => Transformable Text IO (TraceChainSyncServerEvent blk) where
  trTransformer verb tr = trStructured verb tr

-- transform @BlockFetchDecision@
instance
  Show peer =>
  Transformable Text IO
    [ TraceLabelPeer peer
        (FetchDecision [Point header])
    ]
  where
  trTransformer verb tr = trStructured verb tr

-- transform @BlockFetchDecision@
instance
  Show peer =>
  Transformable Text IO
    ( TraceLabelPeer peer
        (TraceFetchClientState header)
    )
  where
  trTransformer verb tr = trStructured verb tr

-- transform @BlockFetchServerEvent@
instance Transformable Text IO (TraceBlockFetchServerEvent blk) where
  trTransformer verb tr = trStructured verb tr

instance
  Transformable Text IO
    ( TraceTxSubmissionInbound
        (GenTxId blk)
        (GenTx blk)
    )
  where
  trTransformer verb tr = trStructured verb tr

instance
  Transformable Text IO
    ( TraceTxSubmissionOutbound
        (GenTxId blk)
        (GenTx blk)
    )
  where
  trTransformer verb tr = trStructured verb tr

instance Transformable Text IO (TraceLocalTxSubmissionServerEvent blk) where
  trTransformer verb tr = trStructured verb tr

instance (Show blk, LedgerSupportsProtocol blk, Show (GenTx blk)) => Transformable Text IO (TraceForgeEvent blk) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

-- transform @SubscriptionTrace@
instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

-- transform @DnsTrace@
instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance Transformable Text IO PoWNodeRpcTrace where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance (HashAlgorithm h, BftCrypto c) => Transformable Text IO (MorphoStateTrace h c) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance (HashAlgorithm h, BftCrypto c) => Transformable Text IO (TimeTravelError (MorphoBlock h c)) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

-- transform @ErrorPolicyTrace@
instance Transformable Text IO (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

-- transform @MuxTrace@
instance
  (Show peer) =>
  Transformable Text IO (WithMuxBearer peer MuxTrace)
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show (ApplyTxErr blk), Show (GenTxId blk), Show (GenTx blk)) =>
  Transformable Text IO (TraceEventMempool blk)
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

-- transform @TraceEvent@
instance
  (Condense (HeaderHash blk), LedgerSupportsProtocol blk) =>
  Transformable Text IO (WithTip blk (ChainDB.TraceEvent blk))
  where
  -- textual output based on the readable ChainDB tracer
  trTransformer _ tr = readableChainDBTracer $ Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack s)
          )

instance Transformable Text IO (TraceTxSubmissionInbound (GenTxId (MorphoBlock h c)) (GenTx (MorphoBlock h c))) where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer) =>
  Transformable Text IO (TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId (MorphoBlock h c)) (GenTx (MorphoBlock h c))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer) =>
  Transformable Text IO (TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId (MorphoBlock h c)) (GenTx (MorphoBlock h c))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (TxSubmission (GenTxId (MorphoBlock h c)) (GenTx (MorphoBlock h c)))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer, HashAlgorithm h, BftCrypto c) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised (MorphoBlock h c)))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer, HashAlgorithm h, BftCrypto c) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (BlockFetch (MorphoBlock h c))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer, HashAlgorithm h, BftCrypto c) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (ChainSync (SerialisedHeader (MorphoBlock h c)) (Tip (MorphoBlock h c)))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer, HashAlgorithm h, BftCrypto c) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header (MorphoBlock h c)) (Tip (MorphoBlock h c)))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer, HashAlgorithm h, BftCrypto c) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (ChainSync (Serialised (MorphoBlock h c)) (Tip (MorphoBlock h c)))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer, HashAlgorithm h, BftCrypto c) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (LocalTxSubmission (GenTx (MorphoBlock h c)) (MorphoError (MorphoBlock h c)))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

instance
  (Show peer, HashAlgorithm h, BftCrypto c) =>
  Transformable Text IO (TraceLabelPeer peer (TraceSendRecv (LocalStateQuery (MorphoBlock h c) (Query (MorphoBlock h c)))))
  where
  trTransformer _ tr = Tracer $ \s ->
    traceWith tr =<< (mempty,)
      <$> ( LogObject <$> pure mempty
              <*> (mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s))
              <*> pure (LogMessage $ pack $ show s)
          )

-- | tracer transformer to text messages for TraceEvents
-- Converts the trace events from the ChainDB that we're interested in into
-- human-readable trace messages.
readableChainDBTracer ::
  forall m blk.
  (Monad m, Condense (HeaderHash blk), LedgerSupportsProtocol blk) =>
  Tracer m String ->
  Tracer m (WithTip blk (ChainDB.TraceEvent blk))
readableChainDBTracer tracer = Tracer $ \case
  WithTip tip (ChainDB.TraceAddBlockEvent ev) -> case ev of
    ChainDB.IgnoreBlockOlderThanK pt ->
      tr $ WithTip tip $
        "Ignoring block older than K: " <> condense pt
    ChainDB.IgnoreBlockAlreadyInVolDB pt ->
      tr $ WithTip tip $
        "Ignoring block already in DB: " <> condense pt
    ChainDB.IgnoreInvalidBlock pt _reason ->
      tr $ WithTip tip $
        "Ignoring previously seen invalid block: " <> condense pt
    ChainDB.BlockInTheFuture pt slot ->
      tr $ WithTip tip $
        "Ignoring block from future: " <> condense pt <> ", slot " <> condense slot
    ChainDB.StoreButDontChange pt ->
      tr $ WithTip tip $
        "Ignoring block: " <> condense pt
    ChainDB.TryAddToCurrentChain pt ->
      tr $ WithTip tip $
        "Block fits onto the current chain: " <> condense pt
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
        tr $ WithTip tip $
          "Invalid block " <> condense pt <> ": " <> show err
      ChainDB.InvalidCandidate c ->
        tr $ WithTip tip $
          "Invalid candidate " <> condense (AF.headPoint c)
      ChainDB.ValidCandidate c ->
        tr $ WithTip tip $
          "Valid candidate " <> condense (AF.headPoint c)
      ChainDB.CandidateContainsFutureBlocks c _ ->
        tr $ WithTip tip $
          "Header wrom the future" <> condense (AF.headPoint c)
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c _ ->
        tr $ WithTip tip $
          "Header wrom the future exceeding the clock skew" <> condense (AF.headPoint c)
    ChainDB.AddedBlockToVolDB pt _ _ ->
      tr $ WithTip tip $
        "Chain added block " <> condense pt
    ChainDB.AddedBlockToQueue pt _ ->
      tr $ WithTip tip $
        "Block added to the queue" <> condense pt
    ChainDB.TrySwitchToAFork pt _ ->
      tr $ WithTip tip $
        "Try to switch to a fork" <> condense pt
    ChainDB.AddedToCurrentChain {} ->
      tr $ WithTip tip $
        "Extended our current chain"
    ChainDB.SwitchedToAFork {} ->
      tr $ WithTip tip $
        "Switched to a new fork"
    ChainDB.ChainSelectionForFutureBlock pt ->
      tr $ WithTip tip $
        "Chain selection for future block" <> condense pt
  -- TODO: check out chaindb constructors to pattern match on them.
  WithTip tip (ChainDB.TraceLedgerReplayEvent ev) -> case ev of
    LedgerDB.ReplayFromGenesis _replayTo ->
      tr $
        WithTip
          tip
          "Replaying ledger from genesis"
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
      tr $ WithTip tip $
        "Replaying ledger from snapshot " <> show snap <> " at "
          <> condense tip'
    LedgerDB.ReplayedBlock _r _replayTo ->
      tr $ WithTip tip $
        "Replayed block"
  WithTip tip (ChainDB.TraceLedgerEvent ev) -> case ev of
    LedgerDB.TookSnapshot snap pt ->
      tr $ WithTip tip $
        "Took ledger snapshot " <> show snap <> " at " <> condense pt
    LedgerDB.DeletedSnapshot snap ->
      tr $ WithTip tip $
        "Deleted old snapshot " <> show snap
    LedgerDB.InvalidSnapshot snap failure ->
      tr $ WithTip tip $
        "Invalid snapshot " <> show snap <> show failure
  WithTip tip (ChainDB.TraceCopyToImmDBEvent ev) -> case ev of
    ChainDB.CopiedBlockToImmDB pt ->
      tr $ WithTip tip $
        "Copied block " <> condense pt <> " to the ImmutableDB"
    ChainDB.NoBlocksToCopyToImmDB ->
      tr $
        WithTip
          tip
          "There are no blocks to copy to the ImmutableDB"
  WithTip tip (ChainDB.TraceGCEvent ev) -> case ev of
    ChainDB.PerformedGC slot ->
      tr $ WithTip tip $
        "Performed a garbage collection for " <> condense slot
    ChainDB.ScheduledGC slot _difft ->
      tr $ WithTip tip $
        "Scheduled a garbage collection for " <> condense slot
  WithTip tip (ChainDB.TraceOpenEvent ev) -> case ev of
    ChainDB.OpenedDB immTip tip' ->
      tr $ WithTip tip $
        "Opened db with immutable tip at " <> condense immTip
          <> " and tip "
          <> condense tip'
    ChainDB.ClosedDB immTip tip' ->
      tr $ WithTip tip $
        "Closed db with immutable tip at " <> condense immTip
          <> " and tip "
          <> condense tip'
    ChainDB.OpenedImmDB immTip epoch ->
      tr $ WithTip tip $
        "Opened imm db with immutable tip at " <> condense immTip
          <> " and epoch "
          <> show epoch
    ChainDB.OpenedVolDB -> tr $ WithTip tip "Opened vol db"
    ChainDB.OpenedLgrDB -> tr $ WithTip tip "Opened lgr db"
  WithTip tip (ChainDB.TraceReaderEvent ev) -> case ev of
    ChainDB.NewReader -> tr $ WithTip tip "New reader"
    ChainDB.ReaderNoLongerInMem _ -> tr $ WithTip tip "ReaderNoLongerInMem"
    ChainDB.ReaderSwitchToMem _ _ -> tr $ WithTip tip "ReaderSwitchToMem"
    ChainDB.ReaderNewImmIterator _ _ -> tr $ WithTip tip "ReaderNewImmIterator"
  WithTip tip (ChainDB.TraceInitChainSelEvent ev) -> case ev of
    ChainDB.InitChainSelValidation _ -> tr $ WithTip tip "InitChainSelValidation"
  WithTip tip (ChainDB.TraceIteratorEvent ev) -> case ev of
    ChainDB.StreamFromVolDB _ _ _ -> tr $ WithTip tip "StreamFromVolDB"
    _ -> pure () -- TODO add more iterator events
  WithTip tip (ChainDB.TraceImmDBEvent _ev) -> tr $ WithTip tip "TraceImmDBEvent"
  WithTip tip _ -> tr $ WithTip tip $ ""
  where
    tr :: WithTip blk String -> m ()
    tr = traceWith (contramap (showWithTip id) tracer)

-- | instances of @ToObject@
instance ToObject (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mkObject
      [ "kind" .= String "WithIPList SubscriptionTrace",
        "localAddresses" .= show localAddresses,
        "dests" .= show dests,
        "event" .= show ev
      ]

instance ToObject (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mkObject
      [ "kind" .= String "SubscriptionTrace",
        "domain" .= show dom,
        "event" .= show ev
      ]

instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mkObject
      [ "kind" .= String "DnsTrace",
        "domain" .= show dom,
        "event" .= show ev
      ]

instance (HashAlgorithm h, BftCrypto c) => ToObject (MorphoStateTrace h c) where
  toObject _verb (MorphoStateTrace (st@(MorphoState _ _ _ tip))) =
    mkObject
      [ "kind" .= String "MorphoStateUpdate",
        "state" .= show st,
        "tip" .= showTip NormalVerbosity tip
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
        "error" .= show err
      ]
  toObject _verb (RpcResponseParseError op err) =
    mkObject
      [ "kind" .= String "RpcResponseParseError",
        "operation" .= show op,
        "error" .= show err
      ]

instance ToObject (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject
      [ "kind" .= String "ErrorPolicyTrace",
        "address" .= show addr,
        "event" .= show ev
      ]

instance
  (Show peer) =>
  ToObject (WithMuxBearer peer MuxTrace)
  where
  toObject _verb (WithMuxBearer b ev) =
    mkObject
      [ "kind" .= String "MuxTrace",
        "bearer" .= show b,
        "event" .= show ev
      ]

instance
  (Condense (HeaderHash blk), LedgerSupportsProtocol blk) =>
  ToObject (WithTip blk (ChainDB.TraceEvent blk))
  where
  -- example: turn off any tracing of @TraceEvent@s when minimal verbosity level is set
  -- toObject MinimalVerbosity _ = emptyObject -- no output
  toObject verb (WithTip tip ev) =
    let evobj = toObject verb ev
     in if evobj == emptyObject
          then emptyObject
          else
            mkObject
              [ "kind" .= String "TraceEvent",
                "tip" .= showTip MinimalVerbosity tip,
                "event" .= evobj
              ]

instance ToObject SlotNo where
  toObject _verb slot =
    mkObject
      [ "kind" .= String "SlotNo",
        "slot" .= toJSON (unSlotNo slot)
      ]

instance
  (Condense (HeaderHash blk)) =>
  ToObject (Point blk)
  where
  toObject MinimalVerbosity p = toObject NormalVerbosity p
  toObject verb p =
    mkObject
      [ "kind" .= String "Tip",
        "tip" .= showTip verb p
      ]

instance
  (Condense (HeaderHash blk), LedgerSupportsProtocol blk) =>
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
            "block" .= showTip verb (AF.headPoint c)
          ]
      ChainDB.ValidCandidate c ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.AddBlockValidation.ValidCandidate",
            "block" .= showTip verb (AF.headPoint c)
          ]
      ChainDB.CandidateContainsFutureBlocks c _ ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.CandidateContainsFutureBlocks",
            "block" .= showTip verb (AF.headPoint c)
          ]
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c _ ->
        mkObject
          [ "kind" .= String "TraceAddBlockEvent.CandidateContainsFutureBlocksExceedingClockSkew",
            "block" .= showTip verb (AF.headPoint c)
          ]
    ChainDB.AddedBlockToVolDB pt (BlockNo bn) _ ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.AddedBlockToVolDB",
          "block" .= toObject verb pt,
          "blockNo" .= show bn
        ]
    ChainDB.AddedBlockToQueue pt _ ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.AddedBlockToQueue",
          "block" .= toObject verb pt
        ]
    ChainDB.AddedToCurrentChain {} ->
      mkObject ["kind" .= String "TraceAddBlockEvent.AddedBlockToQueue"]
    ChainDB.SwitchedToAFork {} ->
      mkObject ["kind" .= String "TraceAddBlockEvent.SwitchedToAFork"]
    ChainDB.ChainSelectionForFutureBlock pt ->
      mkObject
        [ "kind" .= String "TraceAddBlockEvent.ChainSelectionForFutureBlock",
          "block" .= toObject verb pt
        ]
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
    LedgerDB.ReplayedBlock _ _replayTo ->
      mkObject ["kind" .= String "TraceLedgerReplayEvent.ReplayedBlock"]
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
      mkObject ["kind" .= String "TraceGCEvent.PerformedGC"]
    ChainDB.ReaderNoLongerInMem _ ->
      mkObject ["kind" .= String "ReaderNoLongerInMem"]
    ChainDB.ReaderSwitchToMem _ _ ->
      mkObject ["kind" .= String "ReaderSwitchToMem"]
    ChainDB.ReaderNewImmIterator _ _ ->
      mkObject ["kind" .= String "ReaderNewImmIterator"]
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

instance ToObject LedgerDB.DiskSnapshot where
  toObject MinimalVerbosity snap = toObject NormalVerbosity snap
  toObject NormalVerbosity _ = mkObject ["kind" .= String "snapshot"]
  toObject MaximalVerbosity snap =
    mkObject
      [ "kind" .= String "snapshot",
        "snapshot" .= String (pack $ show snap)
      ]

instance
  (Condense (HeaderHash blk), HasHeader (Header blk)) =>
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

instance Condense (HeaderHash blk) => ToObject (TraceChainSyncServerEvent blk) where
  toObject verb ev = case ev of
    TraceChainSyncServerRead tip (AddBlock hdr) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock",
          "tip" .= (String (pack . showTip verb $ getTipPoint tip)),
          "addedBlock" .= (String (pack $ condense hdr))
        ]
    TraceChainSyncServerRead tip (RollBack pt) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerRead.RollBack",
          "tip" .= (String (pack . showTip verb $ getTipPoint tip)),
          "rolledBackBlock" .= (String (pack $ showTip verb pt))
        ]
    TraceChainSyncServerReadBlocked tip (AddBlock hdr) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock",
          "tip" .= (String (pack . showTip verb $ getTipPoint tip)),
          "addedBlock" .= (String (pack $ condense hdr))
        ]
    TraceChainSyncServerReadBlocked tip (RollBack pt) ->
      mkObject
        [ "kind" .= String "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.RollBack",
          "tip" .= (String (pack . showTip verb $ getTipPoint tip)),
          "rolledBackBlock" .= (String (pack $ showTip verb pt))
        ]

instance
  Show peer =>
  ToObject
    [ TraceLabelPeer peer
        (FetchDecision [Point header])
    ]
  where
  toObject MinimalVerbosity _ = emptyObject
  toObject NormalVerbosity lbls =
    mkObject
      [ "kind" .= String "TraceLabelPeer",
        "length" .= String (pack $ show $ length lbls)
      ]
  toObject MaximalVerbosity [] = emptyObject
  toObject MaximalVerbosity (lbl : r) =
    toObject MaximalVerbosity lbl
      <> toObject MaximalVerbosity r

instance
  Show peer =>
  ToObject
    ( TraceLabelPeer peer
        (FetchDecision [Point header])
    )
  where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject
      [ "kind" .= String "FetchDecision",
        "peer" .= show peerid,
        "decision" .= toObject verb a
      ]

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

instance
  Show peer =>
  ToObject
    ( TraceLabelPeer peer
        (TraceFetchClientState header)
    )
  where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject
      [ "kind" .= String "TraceFetchClientState",
        "peer" .= show peerid,
        "state" .= toObject verb a
      ]

instance ToObject (TraceFetchClientState header) where
  toObject _verb (AddedFetchRequest {}) =
    mkObject ["kind" .= String "AddedFetchRequest"]
  toObject _verb (AcknowledgedFetchRequest {}) =
    mkObject ["kind" .= String "AcknowledgedFetchRequest"]
  toObject _verb (CompletedBlockFetch {}) =
    mkObject ["kind" .= String "CompletedBlockFetch"]
  toObject _verb (CompletedFetchBatch {}) =
    mkObject [" kind" .= String "CompletedFetchBatch"]
  toObject _verb (StartedFetchBatch {}) =
    mkObject [" kind" .= String "StartedFetchBatch"]
  toObject _verb (RejectedFetchBatch {}) =
    mkObject [" kind" .= String "RejectedFetchBatch"]

instance ToObject (TraceBlockFetchServerEvent blk) where
  toObject _verb _ =
    mkObject ["kind" .= String "TraceBlockFetchServerEvent"]

instance ToObject (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)) where
  toObject _verb _ =
    mkObject ["kind" .= String "TraceTxSubmissionInbound"]

instance ToObject (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)) where
  toObject _verb _ =
    mkObject ["kind" .= String "TraceTxSubmissionOutbound"]

instance ToObject (TraceLocalTxSubmissionServerEvent blk) where
  toObject _verb _ =
    mkObject ["kind" .= String "TraceLocalTxSubmissionServerEvent"]

instance (LedgerSupportsProtocol blk) => ToObject (TraceForgeEvent blk) where
  toObject _verb (TraceAdoptedBlock slotNo _blk _txs) =
    mkObject
      [ "kind" .= String "TraceAdoptedBlock",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceDidntAdoptBlock slotNo _) =
    mkObject
      [ "kind" .= String "TraceDidntAdoptBlock",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceForgedInvalidBlock slotNo _ invalidBlockReason) =
    mkObject
      [ "kind" .= String "TraceForgedInvalidBlock",
        "slot" .= toJSON (unSlotNo slotNo),
        "reason" .= show invalidBlockReason
      ]
  toObject _verb (TraceStartLeadershipCheck slotNo) =
    mkObject
      [ "kind" .= String "TraceStartLeadershipCheck",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNodeNotLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeNotLeader",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNodeCannotLead slotNo _) =
    mkObject
      [ "kind" .= String "TraceNodeCannotLead",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNoLedgerState slotNo _) =
    mkObject
      [ "kind" .= String "TraceNoLedgerState",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNoLedgerView slotNo _) =
    mkObject
      [ "kind" .= String "TraceNoLedgerView",
        "slot" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceBlockFromFuture curSlotNo futSlotNo) =
    mkObject
      [ "kind" .= String "TraceBlockFromFuture",
        "current_slot" .= toJSON (unSlotNo curSlotNo),
        "block_slot" .= toJSON (unSlotNo futSlotNo)
      ]
  toObject _verb (TraceSlotIsImmutable slotNo _ _) =
    mkObject
      [ "kind" .= String "TraceSlotIsImmutable",
        "slotNo" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceNodeIsLeader slotNo) =
    mkObject
      [ "kind" .= String "TraceNodeIsLeader",
        "slotNo" .= toJSON (unSlotNo slotNo)
      ]
  toObject _verb (TraceForgedBlock slotNo _ _ _) =
    mkObject
      [ "kind" .= String "TraceForgedBlock",
        "slotNo" .= toJSON (unSlotNo slotNo)
      ]

--toObject _verb (TraceNodeNotLeader slotNo) =
--  mkObject
--      [ "kind"    .= String "TraceNodeNotLeader"
--      , "slot"    .= toJSON (unSlotNo slotNo)
--      ]
--toObject _verb (TraceNodeNotLeader slotNo) =
--  mkObject
--      [ "kind"    .= String "TraceNodeNotLeader"
--      , "slot"    .= toJSON (unSlotNo slotNo)
--      ]

instance ToObject (RealPoint blk) where
  toObject _verb (RealPoint slotNo _headerHash) =
    mkObject
      [ "kind" .= String "RealPoint",
        "slot" .= toJSON (unSlotNo slotNo)
      ]

-- | HasPrivacyAnnotation instances
--   Note: defaults to public
instance HasPrivacyAnnotation (TraceChainSyncClientEvent blk)

instance HasPrivacyAnnotation (TraceChainSyncServerEvent blk)

instance HasPrivacyAnnotation (TraceForgeEvent blk)

instance HasPrivacyAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceFetchClientState header))

instance HasPrivacyAnnotation (TraceBlockFetchServerEvent blk)

instance HasPrivacyAnnotation (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))

instance HasPrivacyAnnotation (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))

instance HasPrivacyAnnotation (TraceLocalTxSubmissionServerEvent blk)

instance HasPrivacyAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))

instance HasPrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))

instance HasPrivacyAnnotation (WithDomainName DnsTrace)

instance HasPrivacyAnnotation PoWNodeRpcTrace

instance HasPrivacyAnnotation (MorphoStateTrace h c)

instance HasPrivacyAnnotation (TimeTravelError blk)

instance HasPrivacyAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace)

instance HasPrivacyAnnotation (WithMuxBearer peer MuxTrace)

instance HasPrivacyAnnotation (WithTip blk (ChainDB.TraceEvent blk))

instance HasPrivacyAnnotation (TraceEventMempool blk)

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised (MorphoBlock h c)))))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv (BlockFetch (MorphoBlock h c))))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv (ChainSync (SerialisedHeader (MorphoBlock h c)) (Tip (MorphoBlock h c)))))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header (MorphoBlock h c)) (Tip (MorphoBlock h c)))))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv (ChainSync (Serialised (MorphoBlock h c)) (Tip (MorphoBlock h c)))))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv ((LocalTxSubmission (GenTx (MorphoBlock h c)) (MorphoError (MorphoBlock h c))))))

instance HasPrivacyAnnotation (TraceLabelPeer peer (TraceSendRecv (LocalStateQuery (MorphoBlock h c) (Query (MorphoBlock h c)))))

-- | HasSeverityAnnotation instances
--   Note: defaults to debug
instance HasSeverityAnnotation (TraceChainSyncClientEvent blk)

instance HasSeverityAnnotation (TraceChainSyncServerEvent blk)

instance HasSeverityAnnotation (TraceForgeEvent blk)

instance HasSeverityAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceFetchClientState header))

instance HasSeverityAnnotation (TraceBlockFetchServerEvent blk)

instance HasSeverityAnnotation (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))

instance HasSeverityAnnotation (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))

instance HasSeverityAnnotation (TraceLocalTxSubmissionServerEvent blk)

instance HasSeverityAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))

instance HasSeverityAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))

instance HasSeverityAnnotation (WithDomainName DnsTrace)

instance HasSeverityAnnotation PoWNodeRpcTrace

instance HasSeverityAnnotation (MorphoStateTrace h c)

instance HasSeverityAnnotation (TimeTravelError blk)

instance HasSeverityAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace)

instance HasSeverityAnnotation (WithMuxBearer peer MuxTrace)

instance HasSeverityAnnotation (WithTip blk (ChainDB.TraceEvent blk))

instance HasSeverityAnnotation (TraceEventMempool blk)

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (TxSubmission (GenTxId blk) (GenTx blk))))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (BlockFetch (Serialised (MorphoBlock h c)))))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (BlockFetch (MorphoBlock h c))))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (ChainSync (SerialisedHeader (MorphoBlock h c)) (Tip (MorphoBlock h c)))))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (ChainSync (Header (MorphoBlock h c)) (Tip (MorphoBlock h c)))))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (ChainSync (Serialised (MorphoBlock h c)) (Tip (MorphoBlock h c)))))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (LocalTxSubmission (GenTx (MorphoBlock h c)) (MorphoError (MorphoBlock h c)))))

instance HasSeverityAnnotation (TraceLabelPeer peer (TraceSendRecv (LocalStateQuery (MorphoBlock h c) (Query (MorphoBlock h c)))))
