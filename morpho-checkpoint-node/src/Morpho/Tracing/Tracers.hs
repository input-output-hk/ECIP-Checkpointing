{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Tracing.Tracers
  ( ChainInformation (..),
    Tracers (..),
    chainInformation,
    mkTracers,
    withTip,
  )
where

import Cardano.BM.Data.Aggregated
import Cardano.BM.Data.LogItem
import Cardano.BM.Data.Tracer (WithSeverity (..), annotateSeverity, mkObject, showTracing, trStructured)
import Cardano.BM.Data.Transformers
import Cardano.BM.Trace
import Cardano.BM.Tracing
import Cardano.Crypto.DSIGN.Class
import Cardano.Prelude hiding (show)
import qualified Cardano.Prelude (show)
import Codec.CBOR.Read (DeserialiseFailure)
import Control.Monad.Class.MonadSTM hiding (atomically)
import Control.Monad.Class.MonadTime
import Control.Tracer.Transformers
import Data.Aeson
import qualified Data.Text as Text
import Morpho.Config.Types
import Morpho.Ledger.Block
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.Update
import Morpho.Node.RunNode ()
import Morpho.Tracing.TracingOrphanInstances
import Morpho.Tracing.Types
import Network.Mux.Trace (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
import Ouroboros.Consensus.BlockchainTime.WallClock.Util
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Mempool.API
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Types as ChainDB
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.BlockFetch.ClientState
import Ouroboros.Network.BlockFetch.Decision
import Ouroboros.Network.Diffusion
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
import qualified Ouroboros.Network.NodeToNode as NtN
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.Point
import Ouroboros.Network.Snocket (LocalAddress)
import Prelude (String, show)

data Tracers peer localPeer h c = Tracers
  { -- | Used for top-level morpho traces during initialization
    morphoInitTracer :: Tracer IO MorphoInitTrace,
    -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
    chainDBTracer :: Tracer IO (ChainDB.TraceEvent (MorphoBlock h c)),
    -- | Consensus-specific tracers.
    consensusTracers :: Consensus.Tracers IO peer localPeer (MorphoBlock h c),
    -- | Trace the IP subscription manager (flag '--trace-ip-subscription' will turn on textual output)
    ipSubscriptionTracer :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr)),
    -- | Trace the DNS subscription manager (flag '--trace-dns-subscription' will turn on textual output)
    dnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr)),
    -- | Trace the DNS resolver (flag '--trace-dns-resolver' will turn on textual output)
    dnsResolverTracer :: Tracer IO (WithDomainName DnsTrace),
    -- | Trace Mux
    muxTracer :: Tracer IO (WithMuxBearer peer MuxTrace),
    -- | Trace error policy resolution (flag '--trace-error-policy' will turn on textual output)
    muxLocalTracer :: Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace),
    errorPolicyTracer :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace),
    powNodeRpcTracer :: Tracer IO PoWNodeRpcTrace,
    extractStateTracer :: Tracer IO (ExtractStateTrace h c),
    timeTravelErrorTracer :: Tracer IO (TimeTravelError (MorphoBlock h c)),
    -- | Chain Tip tracer.
    --
    --   Note: we're currently not using structured logging approach
    --   for the chain Tip tracer. Implementing all the required
    --   typeclasses is a hassle and we're not mining these
    --   structured logs anyways, we are just plain-text quering
    --   them. Modeling the chain tip hash as Text should be good
    --   enough for now.
    chainTipTracer :: Tracer IO Text,
    nodeToNodeTracers :: NodeToNode.Tracers IO peer (MorphoBlock h c) DeserialiseFailure,
    nodeToClientTracers :: NodeToClient.Tracers IO localPeer (MorphoBlock h c) DeserialiseFailure,
    handshakeTracer :: Tracer IO NtN.HandshakeTr,
    handshakeLocalTracer :: Tracer IO NtC.HandshakeTr,
    localErrorPolicyTracer :: Tracer IO (NtN.WithAddr NtC.LocalAddress NtN.ErrorPolicyTrace),
    acceptPolicyTracer :: Tracer IO AcceptConnectionsPolicyTrace,
    diffusionInitializationTracer :: Tracer IO DiffusionInitializationTracer,
    ledgerPeersTracer :: Tracer IO TraceLedgerPeers
  }

-- | get information about a chain fragment
data ChainInformation = ChainInformation
  { slots :: Word64,
    blocks :: Word64,
    -- | the actual number of blocks created over the maximum
    -- expected number of blocks that could be created
    density :: Rational
  }

data ForgeTracers = ForgeTracers
  { ftForged :: Trace IO Text,
    ftNodeCannotForge :: Trace IO Text,
    ftAdopted :: Trace IO Text,
    ftDidntAdoptBlock :: Trace IO Text,
    ftForgedInvalid :: Trace IO Text,
    ftTraceNodeNotLeader :: Trace IO Text,
    ftTraceBlockFromFuture :: Trace IO Text,
    ftTraceSlotIsImmutable :: Trace IO Text,
    ftTraceNodeIsLeader :: Trace IO Text,
    ftTraceStartLeadershipCheck :: Trace IO Text,
    ftTraceNoLedgerState :: Trace IO Text,
    ftTraceNoLedgerView :: Trace IO Text,
    ftTraceBlockContext :: Trace IO Text,
    ftTraceLedgerState :: Trace IO Text,
    ftTraceLedgerView :: Trace IO Text,
    ftTraceForgeStateUpdateError :: Trace IO Text
  }

-- | Definition of the measurement datatype for the transactions.
data MeasureTxs blk
  = MeasureTxsTimeStart [GenTx blk] Word Word Time -- num txs, total size in bytes
  | MeasureTxsTimeStop SlotNo blk [GenTx blk]

deriving instance (Eq blk, Eq (GenTx blk)) => Eq (MeasureTxs blk)

deriving instance (Show blk, Show (GenTx blk)) => Show (MeasureTxs blk)

instance HasPrivacyAnnotation (MeasureTxs blk)

instance HasSeverityAnnotation (MeasureTxs blk)

instance Transformable Text IO (MeasureTxs blk) where
  trTransformer = trStructured

-- TODO(KS): Clarify the structure of the type.
instance ToObject (MeasureTxs blk) where
  toObject _verb _ =
    mkObject
      [ "kind" .= String "MeasureTxsTimeStart"
      ]

-- | Generates all the tracers necessary for the checkpointing node.
--
-- Note: the constraint on the morpho block is necessary for the
-- Condense implementations.
mkTracers ::
  forall peer localPeer blk h c.
  ( Show peer,
    Show localPeer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c),
    LedgerSupportsProtocol blk
  ) =>
  TraceOptions ->
  Trace IO Text ->
  IO (Tracers peer localPeer h c)
mkTracers traceOptions tracer = do
  -- We probably don't want to pay the extra IO cost per-counter-increment. -- sk
  staticMetaCC <- mkLOMeta Info Confidential
  let name :: Text = "metrics.Forge"
  forgeTracers <-
    ForgeTracers
      <$> counting (liftCounting staticMetaCC name "forged" tracer)
      <*> counting (liftCounting staticMetaCC name "could-not-forge" tracer)
      <*> counting (liftCounting staticMetaCC name "adopted" tracer)
      <*> counting (liftCounting staticMetaCC name "didnt-adopt" tracer)
      <*> counting (liftCounting staticMetaCC name "forged-invalid" tracer)
      <*> counting (liftCounting staticMetaCC name "node-not-leader" tracer)
      <*> counting (liftCounting staticMetaCC name "block-from-future" tracer)
      <*> counting (liftCounting staticMetaCC name "slot-is-immutable" tracer)
      <*> counting (liftCounting staticMetaCC name "node-is-leader" tracer)
      <*> counting (liftCounting staticMetaCC name "trace-start-leadershipCheck" tracer)
      <*> counting (liftCounting staticMetaCC name "trace-no-ledger-state" tracer)
      <*> counting (liftCounting staticMetaCC name "trace-no-ledger-view" tracer)
      <*> counting (liftCounting staticMetaCC name "block-context" tracer)
      <*> counting (liftCounting staticMetaCC name "ledger-state" tracer)
      <*> counting (liftCounting staticMetaCC name "ledger-view" tracer)
      <*> counting (liftCounting staticMetaCC name "forge-state-update-error" tracer)
  pure
    Tracers
      { morphoInitTracer =
          annotateSeverity $
            toLogObject' tracingVerbosity $
              appendName "MorphoInit" tracer,
        chainDBTracer =
          tracerOnOff (traceChainDB traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "ChainDB" tracer,
        consensusTracers =
          mkConsensusTracers forgeTracers traceOptions,
        ipSubscriptionTracer =
          tracerOnOff (traceIpSubscription traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "IpSubscription" tracer,
        dnsSubscriptionTracer =
          tracerOnOff (traceDnsSubscription traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "DnsSubscription" tracer,
        dnsResolverTracer =
          tracerOnOff (traceDnsResolver traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "DnsResolver" tracer,
        muxTracer =
          tracerOnOff (traceMux traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "Mux" tracer,
        muxLocalTracer =
          tracerOnOff (traceMux traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "LocalMux" tracer,
        errorPolicyTracer =
          tracerOnOff (traceErrorPolicy traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "ErrorPolicy" tracer,
        extractStateTracer =
          tracerOnOff (traceLedgerState traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "ExtractState" tracer,
        powNodeRpcTracer =
          tracerOnOff (tracePoWNodeRpc traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "PoWNodeRpc" tracer,
        timeTravelErrorTracer =
          tracerOnOff (traceTimeTravelError traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "TimeTravelError" tracer,
        chainTipTracer =
          tracerOnOff (traceTimeTravelError traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "chainTipTracer" tracer,
        nodeToNodeTracers =
          nodeToNodeTracers' traceOptions tracer,
        nodeToClientTracers =
          nodeToClientTracers' traceOptions tracer,
        handshakeTracer =
          tracerOnOff (traceHandshake traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "Handshake" tracer,
        handshakeLocalTracer =
          tracerOnOff (traceHandshake traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "HandshakeLocal" tracer,
        localErrorPolicyTracer =
          tracerOnOff (traceErrorPolicy traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "Handshake" tracer,
        acceptPolicyTracer =
          tracerOnOff (traceErrorPolicy traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "Handshake" tracer,
        diffusionInitializationTracer =
          tracerOnOff (traceErrorPolicy traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "DiffusionInitialization" tracer,
        ledgerPeersTracer =
          tracerOnOff (traceErrorPolicy traceOptions) $
            annotateSeverity $
              toLogObject' tracingVerbosity $
                appendName "LedgerPeers" tracer
      }
  where
    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceVerbosity traceOptions
    teeTraceBlockFetchDecision ::
      TracingVerbosity ->
      Trace IO Text ->
      Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision _ tr = Tracer $ \ev -> do
      traceWith (teeTraceBlockFetchDecision' tr) ev
    teeTraceBlockFetchDecision' ::
      Trace IO Text ->
      Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision' tr =
      Tracer $ \(WithSeverity _ peers) -> do
        meta <- mkLOMeta Notice Confidential
        let tr' = appendName "peers" tr
        traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)
    mempoolTraceTransformer ::
      Trace IO Text ->
      Tracer IO (TraceEventMempool blk)
    mempoolTraceTransformer tr = Tracer $ \mempoolEvent -> do
      let tr' = appendName "metrics" tr
          (n, tot) = case mempoolEvent of
            TraceMempoolAddedTx _tx0 _ tot0 -> (1, tot0)
            TraceMempoolRejectedTx _tx0 _ tot0 -> (1, tot0)
            TraceMempoolRemoveTxs txs0 tot0 -> (length txs0, tot0)
            TraceMempoolManuallyRemovedTxs txs0 txs1 tot0 -> (length txs0 + length txs1, tot0)
          logValue1 :: LOContent a
          logValue1 = LogValue "txsInMempool" $ PureI $ fromIntegral (msNumTxs tot)
          logValue2 :: LOContent a
          logValue2 = LogValue "txsProcessed" $ PureI $ fromIntegral n
      meta <- mkLOMeta Info Confidential
      traceNamedObject tr' (meta, logValue1)
      traceNamedObject tr' (meta, logValue2)
    mempoolTracer :: Tracer IO (TraceEventMempool blk)
    mempoolTracer = Tracer $ \ev -> do
      traceWith (mempoolTraceTransformer tracer) ev
      let tr = appendName "Mempool" tracer
      traceWith (mpTracer tr) ev
    mpTracer :: Trace IO Text -> Tracer IO (TraceEventMempool blk)
    mpTracer tr = annotateSeverity $ toLogObject' tracingVerbosity tr
    forgeTracer :: ForgeTracers -> TraceOptions -> Tracer IO (Consensus.TraceLabelCreds (Consensus.TraceForgeEvent blk))
    forgeTracer forgeTracers traceOpts = Tracer $ \(Consensus.TraceLabelCreds _ ev) -> do
      traceWith (measureTxsEnd tracer) ev
      traceWith consensusForgeTracer ev
      where
        -- The consensus tracer.
        consensusForgeTracer =
          tracerOnOff (traceForge traceOpts) $
            annotateSeverity $
              teeForge forgeTracers tracingVerbosity $
                appendName "Forge" tracer
    teeForge ::
      ForgeTracers ->
      TracingVerbosity ->
      Trace IO Text ->
      Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk))
    teeForge ft tverb tr = Tracer $ \ev -> do
      traceWith (teeForge' tr) ev
      flip traceWith ev $
        fanning $ \(WithSeverity _ e) ->
          case e of
            Consensus.TraceForgedBlock {} -> teeForge' (ftForged ft)
            Consensus.TraceNodeCannotForge {} -> teeForge' (ftNodeCannotForge ft)
            Consensus.TraceAdoptedBlock {} -> teeForge' (ftAdopted ft)
            Consensus.TraceDidntAdoptBlock {} -> teeForge' (ftDidntAdoptBlock ft)
            Consensus.TraceForgedInvalidBlock {} -> teeForge' (ftForgedInvalid ft)
            Consensus.TraceStartLeadershipCheck {} -> teeForge' (ftTraceStartLeadershipCheck ft)
            Consensus.TraceNodeNotLeader {} -> teeForge' (ftTraceNodeNotLeader ft)
            Consensus.TraceNoLedgerState {} -> teeForge' (ftTraceNoLedgerState ft)
            Consensus.TraceNoLedgerView {} -> teeForge' (ftTraceNoLedgerView ft)
            Consensus.TraceBlockFromFuture {} -> teeForge' (ftTraceBlockFromFuture ft)
            Consensus.TraceSlotIsImmutable {} -> teeForge' (ftTraceSlotIsImmutable ft)
            Consensus.TraceNodeIsLeader {} -> teeForge' (ftTraceNodeIsLeader ft)
            Consensus.TraceBlockContext {} -> teeForge' (ftTraceBlockContext ft)
            Consensus.TraceLedgerState {} -> teeForge' (ftTraceLedgerState ft)
            Consensus.TraceLedgerView {} -> teeForge' (ftTraceLedgerView ft)
            Consensus.TraceForgeStateUpdateError {} -> teeForge' (ftTraceForgeStateUpdateError ft)
      traceWith (toLogObject' tverb tr) ev
    teeForge' ::
      Trace IO Text ->
      Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk))
    teeForge' tr =
      Tracer $ \(WithSeverity _ ev) -> do
        meta <- mkLOMeta Info Confidential
        traceNamedObject (appendName "metrics" tr) . (meta,) $
          case ev of
            Consensus.TraceForgedBlock slot _ _ _ ->
              LogValue "forgedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNodeCannotForge slot _ ->
              LogValue "node Could not forge" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceAdoptedBlock slot _ _ ->
              LogValue "adoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceDidntAdoptBlock slot _ ->
              LogValue "notAdoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceForgedInvalidBlock slot _ _ ->
              LogValue "forgedInvalidSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceStartLeadershipCheck slot ->
              LogValue "startLeadershipCheck" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNodeNotLeader slot ->
              LogValue "nodeNotLeader" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNoLedgerState slot _ ->
              LogValue "noLedgerState" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNoLedgerView slot _ ->
              LogValue "noLedgerView" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceBlockFromFuture slot _ ->
              LogValue "blockFromFuture" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceSlotIsImmutable slot _ _ ->
              LogValue "slotIsImmutable" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceNodeIsLeader slot ->
              LogValue "nodeIsLeader" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceBlockContext slot _ _ ->
              LogValue "blockContext" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceLedgerState slot _ ->
              LogValue "ledgerState" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceLedgerView slot ->
              LogValue "ledgerView" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceForgeStateUpdateError slot _ ->
              LogValue "forgeStateUpdateError" $ PureI $ fromIntegral $ unSlotNo slot
    mkConsensusTracers :: ForgeTracers -> TraceOptions -> Consensus.Tracers IO peer localPeer blk
    mkConsensusTracers forgeTracers traceOpts =
      Consensus.Tracers
        { Consensus.chainSyncClientTracer =
            tracerOnOff (traceChainSyncClient traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "ChainSyncClient" tracer,
          Consensus.chainSyncServerHeaderTracer =
            tracerOnOff (traceChainSyncHeaderServer traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "ChainSyncHeaderServer" tracer,
          Consensus.chainSyncServerBlockTracer =
            tracerOnOff (traceChainSyncBlockServer traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "ChainSyncBlockServer" tracer,
          Consensus.blockFetchDecisionTracer =
            tracerOnOff (traceBlockFetchDecisions traceOpts) $
              annotateSeverity $
                teeTraceBlockFetchDecision tracingVerbosity $
                  appendName "BlockFetchDecision" tracer,
          Consensus.blockFetchClientTracer =
            tracerOnOff (traceBlockFetchClient traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "BlockFetchClient" tracer,
          Consensus.blockFetchServerTracer =
            tracerOnOff (traceBlockFetchServer traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "BlockFetchServer" tracer,
          Consensus.txInboundTracer =
            tracerOnOff (traceTxInbound traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "TxInbound" tracer,
          Consensus.txOutboundTracer =
            tracerOnOff (traceTxOutbound traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "TxOutbound" tracer,
          Consensus.localTxSubmissionServerTracer =
            tracerOnOff (traceLocalTxSubmissionServer traceOpts) $
              toLogObject' tracingVerbosity $
                appendName "LocalTxSubmissionServer" tracer,
          Consensus.mempoolTracer =
            tracerOnOff (traceMempool traceOpts) mempoolTracer,
          Consensus.forgeTracer =
            forgeTracer forgeTracers traceOpts,
          Consensus.blockchainTimeTracer = Tracer $ \ev ->
            traceWith (toLogObject tracer) (readableTraceBlockchainTimeEvent ev),
          -- TODO: trace the forge state if we add any.
          Consensus.forgeStateInfoTracer = Tracer $ const mempty,
          -- TODO: Trace this
          Consensus.keepAliveClientTracer = Tracer $ const mempty
        }
    readableTraceBlockchainTimeEvent :: TraceBlockchainTimeEvent UTCTime -> Text
    readableTraceBlockchainTimeEvent ev = case ev of
      TraceStartTimeInTheFuture (SystemStart start) toWait ->
        "Waiting " <> Cardano.Prelude.show toWait <> " until genesis start time at " <> Cardano.Prelude.show start
      TraceSystemClockMovedBack _ _ -> "System clock moved back an acceptable time span"
      TraceCurrentSlotUnknown _ _ -> "Current slot is not yet known"

chainInformation ::
  forall block.
  AF.HasHeader block =>
  AF.AnchoredFragment block ->
  ChainInformation
chainInformation frag =
  ChainInformation
    { slots = slotN,
      blocks = blockN,
      density = calcDensity blockD slotD
    }
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD =
      slotN
        - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo . fromMaybe 1 $ withOriginToMaybe (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _ -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b

-- | Transformer for the end of the transaction, when the transaction was added to the
-- block and the block was forged.
measureTxsEnd :: Trace IO Text -> Tracer IO (Consensus.TraceForgeEvent blk)
measureTxsEnd tracer = measureTxsEndInter $ toLogObject tracer
  where
    measureTxsEndInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (Consensus.TraceForgeEvent blk)
    measureTxsEndInter tracer' = Tracer $ \case
      Consensus.TraceAdoptedBlock slotNo blk txs -> traceWith tracer' (MeasureTxsTimeStop slotNo blk txs)
      _ -> pure ()

-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
withTip ::
  TVar IO (Point blk) ->
  Tracer IO (WithTip blk a) ->
  Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
  tip <- atomically $ readTVar varTip
  traceWith (contramap (WithTip tip) tr) msg

--------------------------------------------------------------------------------
-- NodeToNode Tracers
--------------------------------------------------------------------------------

nodeToNodeTracers' ::
  ( Show peer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c
  ) =>
  TraceOptions ->
  Trace IO Text ->
  NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' traceOptions tracer =
  let tVerb = traceVerbosity traceOptions
   in NodeToNode.Tracers
        { NodeToNode.tChainSyncTracer =
            tracerOnOff (traceChainSyncProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "ChainSyncProtocol" tracer,
          NodeToNode.tChainSyncSerialisedTracer =
            tracerOnOff (traceChainSyncProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "ChainSyncProtocolSerialised" tracer,
          NodeToNode.tBlockFetchTracer =
            tracerOnOff (traceBlockFetchProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "BlockFetchProtocol" tracer,
          NodeToNode.tBlockFetchSerialisedTracer =
            showOnOff
              (traceBlockFetchProtocolSerialised traceOptions)
              "BlockFetchProtocolSerialised"
              tracer,
          NodeToNode.tTxSubmissionTracer =
            tracerOnOff (traceTxSubmissionProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "TxSubmissionProtocol" tracer,
          NodeToNode.tTxSubmission2Tracer =
            tracerOnOff (traceTxSubmissionProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "TxSubmission2Protocol" tracer
        }

nodeToClientTracers' ::
  ( Show peer,
    blk ~ MorphoBlock h c
  ) =>
  TraceOptions ->
  Trace IO Text ->
  NodeToClient.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToClientTracers' traceOptions tracer =
  let tVerb = traceVerbosity traceOptions
   in NodeToClient.Tracers
        { NodeToClient.tChainSyncTracer =
            tracerOnOff (traceLocalChainSyncProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "LocalChainSyncProtocol" tracer,
          NodeToClient.tTxSubmissionTracer =
            tracerOnOff (traceLocalTxSubmissionProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "LocalTxSubmissionProtocol" tracer,
          NodeToClient.tStateQueryTracer =
            tracerOnOff (traceLocalStateQueryProtocol traceOptions) $
              annotateSeverity $
                toLogObject' tVerb $
                  appendName "LocalStateQueryProtocol" tracer
        }

instance Show a => Show (WithSeverity a) where
  show (WithSeverity _sev a) = show a

-- Turn on/off a tracer depending on what was parsed from the command line.
tracerOnOff :: Bool -> Tracer IO a -> Tracer IO a
tracerOnOff False _ = nullTracer
tracerOnOff True tracer = tracer

showOnOff ::
  (Show a, HasSeverityAnnotation a) =>
  Bool ->
  LoggerName ->
  Trace IO Text ->
  Tracer IO a
showOnOff False _ _ = nullTracer
showOnOff True name trcer =
  annotateSeverity $
    showTracing $
      withName name trcer

withName :: Text -> Trace IO Text -> Tracer IO String
withName name tr = contramap Text.pack $ toLogObject $ appendName name tr
