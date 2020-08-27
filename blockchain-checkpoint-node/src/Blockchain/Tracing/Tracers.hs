{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Blockchain.Tracing.Tracers (
  Tracers(..),
  TraceConstraints,
  mkTracers,
  withTip
) where

import Cardano.Prelude
import Prelude (String)

import Blockchain.Config.Protocol
import Blockchain.Config.Types
import Blockchain.Tracing.ToObjectOrphans
import Blockchain.Tracing.Types
import Blockchain.Ledger.Block

import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Tracer (WithSeverity(..), trStructured, mkObject, addName, annotateSeverity)
import           Cardano.BM.Data.Transformers
import           Cardano.BM.Trace
import           Cardano.BM.Tracing
import           Codec.CBOR.Read
import           Control.Monad.Class.MonadTime (Time, DiffTime, diffTime)
import           Control.Tracer
import           Control.Tracer.Transformers
import           Control.Tracer.Transformers.ObserveOutcome
import           Data.Aeson (Value (..), (.=))
import           Data.Text (pack)
import           Network.Mux.Types
import           Ouroboros.Consensus.BlockchainTime.SlotLengths
import           Ouroboros.Consensus.BlockchainTime.WallClock
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Control.Monad.Class.MonadSTM (TVar, readTVar)
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.Point
import qualified Network.Socket as Socket
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Storage.ChainDB as ChainDB

data Tracers peer blk c ext = Tracers {
      -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
      chainDBTracer         :: Tracer IO (WithTip blk (ChainDB.TraceEvent blk))

      -- | Consensus-specific tracers.
    , consensusTracers      :: Consensus.Tracers IO peer blk

      -- | Tracers for the protocol messages.
    , protocolTracers       :: ProtocolTracers IO peer blk DeserialiseFailure

      -- | Trace the IP subscription manager (flag '--trace-ip-subscription' will turn on textual output)
    , ipSubscriptionTracer  :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))

      -- | Trace the DNS subscription manager (flag '--trace-dns-subscription' will turn on textual output)
    , dnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))

      -- | Trace the DNS resolver (flag '--trace-dns-resolver' will turn on textual output)
    , dnsResolverTracer     :: Tracer IO (WithDomainName DnsTrace)

      -- | Trace error policy resolution (flag '--trace-error-policy' will turn on textual output)
    , errorPolicyTracer     :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)

      -- | Trace the Mux (flag --trace-mux' will turn on textual output)
    , muxTracer             :: Tracer IO (WithMuxBearer peer (MuxTrace NodeToNodeProtocols))
    , rpcTracer     :: Tracer IO RpcTrace
    , morphoStateTracer     :: Tracer IO (MorphoStateTrace c ext)
    , timeTravelErrorTracer :: Tracer IO (TimeTravelErrorTrace c ext)
    }


-- | get information about a chain fragment
data ChainInformation = ChainInformation {
    slots :: Word64,
    blocks :: Word64,
    density :: Rational
    -- ^ the actual number of blocks created over the maximum
    -- expected number of blocks that could be created
  }

data ForgeTracers = ForgeTracers
  { ftForged :: Trace IO Text
  , ftCouldNotForge :: Trace IO Text
  , ftAdopted :: Trace IO Text
  , ftDidntAdoptBlock :: Trace IO Text
  , ftForgedInvalid :: Trace IO Text
  }


-- | Definition of the measurement datatype for the transactions.
data MeasureTxs blk
    = MeasureTxsTimeStart [GenTx blk] Word Word Time  -- num txs, total size in bytes
    | MeasureTxsTimeStop SlotNo blk [GenTx blk] Time

deriving instance (ProtocolLedgerView blk, Eq blk, Eq (GenTx blk)) => Eq (MeasureTxs blk)
deriving instance (ProtocolLedgerView blk, Show blk, Show (GenTx blk)) => Show (MeasureTxs blk)

-- Any Monad m, could be Identity in this case where we have all the data beforehand.
-- The result of this operation is the list of transactions that _made it in the block_
-- and the time it took them to get into the block.
instance (Monad m, Eq (GenTx blk)) => Outcome m (MeasureTxs blk) where
    type IntermediateValue  (MeasureTxs blk)    = [(GenTx blk, Time)]
    type OutcomeMetric      (MeasureTxs blk)    = [(GenTx blk, DiffTime)]

    --classifyObservable     :: a -> m OutcomeProgressionStatus
    classifyObservable = pure . \case
      MeasureTxsTimeStart {}    -> OutcomeStarts
      MeasureTxsTimeStop  {}    -> OutcomeEnds

    --captureObservableValue :: a -> m (IntermediateValue a)
    captureObservableValue (MeasureTxsTimeStart txs _ _ time) =
        pure [(tx, time) | tx <- txs]

    captureObservableValue (MeasureTxsTimeStop _sloNo _blk txs time) =
        pure [(tx, time) | tx <- txs]

    --computeOutcomeMetric   :: a -> IntermediateValue a -> IntermediateValue a -> m (OutcomeMetric a)
    computeOutcomeMetric _ xs ys = pure . computeFinalValues $ computeIntermediateValues xs ys
      where
        -- | Here we filter and match all the transactions that made it into
        -- a block.
        computeIntermediateValues
            :: [(GenTx blk, Time)]
            -> [(GenTx blk, Time)]
            -> [((GenTx blk, Time), (GenTx blk, Time))]
        computeIntermediateValues [] _  = []
        computeIntermediateValues _ []  = []
        --[ (x, y) | x@(xTx, _) <- xs, y@(yTx, _) <- ys, xTx == yTx ]
        computeIntermediateValues xs' ys' = do
            x@(xTx, _) <- xs'
            y@(yTx, _) <- ys'
            guard (xTx == yTx)
            return (x, y)

        -- | From all the transactions that made it into a block we simply
        -- diff the time it took them and associate that time with the transaction
        -- that made it into a block.
        computeFinalValues
            :: [((GenTx blk, Time), (GenTx blk, Time))]
            -> [(GenTx blk, DiffTime)]
        computeFinalValues intermediateValues =
            map (\((blk, timeStart), (_, timeEnd)) -> (blk, diffTime timeEnd timeStart)) intermediateValues


instance Transformable Text IO (MeasureTxs blk) where
  trTransformer _ verb tr = trStructured verb tr

instance DefinePrivacyAnnotation (MeasureTxs blk)
instance DefineSeverity (MeasureTxs blk) where
  defineSeverity _ = Info

-- TODO(KS): Clarify the structure of the type.
instance ToObject (MeasureTxs blk) where
  toObject _verb _ =
    mkObject [ "kind"       .= String "MeasureTxsTimeStart"
             ]


mkTracers :: forall peer blk c ext.
  ( ProtocolLedgerView blk
  , TraceConstraints blk
  , MorphoCrypto c
  , Typeable ext
  , Show peer
  )
  => TraceOptions
  -> Trace IO Text
  -> IO (Tracers peer blk c ext)
mkTracers traceOptions tracer = do
  -- We probably don't want to pay the extra IO cost per-counter-increment. -- sk
  staticMetaCC <- mkLOMeta Critical Confidential
  let name :: [Text] = ["metrics", "Forge"]
  forgeTracers <-
    ForgeTracers
      <$> (counting $ liftCounting staticMetaCC name "forged" tracer)
      <*> (counting $ liftCounting staticMetaCC name "could-not-forge" tracer)
      <*> (counting $ liftCounting staticMetaCC name "adopted" tracer)
      <*> (counting $ liftCounting staticMetaCC name "didnt-adopt" tracer)
      <*> (counting $ liftCounting staticMetaCC name "forged-invalid" tracer)

  pure Tracers
    { chainDBTracer
        = tracerOnOff (traceChainDB traceOptions)
          $ annotateSeverity
          $ teeTraceChainTip StructuredLogging tracingVerbosity
          $ addName "ChainDB" tracer
    , consensusTracers
        = mkConsensusTracers forgeTracers traceOptions
    , protocolTracers
        = mkProtocolsTracers traceOptions
    , ipSubscriptionTracer
        = tracerOnOff (traceIpSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "IpSubscription" tracer
    , dnsSubscriptionTracer
        = tracerOnOff (traceDnsSubscription traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "DnsSubscription" tracer
    , dnsResolverTracer
        = tracerOnOff (traceDnsResolver traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "DnsResolver" tracer
    , errorPolicyTracer
        = tracerOnOff (traceErrorPolicy traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ErrorPolicy" tracer
    , muxTracer
        =  tracerOnOff (traceMux traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "Mux" tracer
    , morphoStateTracer
        = tracerOnOff (traceLedgerState traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "MorphoState" tracer
    , timeTravelErrorTracer
        = tracerOnOff True
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "TimeTravelError" tracer      
    , rpcTracer
        = tracerOnOff (traceRpc traceOptions)
          $ annotateSeverity
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "Rpc" tracer
    }
  where
    -- Turn on/off a tracer depending on what was parsed from the command line.
    tracerOnOff :: Bool -> Tracer IO a -> Tracer IO a
    tracerOnOff False _ = nullTracer
    tracerOnOff True tracer' = tracer'
    tracingVerbosity :: TracingVerbosity
    tracingVerbosity = traceVerbosity traceOptions

    teeTraceChainTip :: TracingFormatting
                     -> TracingVerbosity
                     -> Tracer IO (LogObject Text)
                     -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip tform tverb tr = Tracer $ \ev -> do
        traceWith (teeTraceChainTip' tr) ev
        traceWith (toLogObject' tform tverb tr) ev
    teeTraceChainTip' :: Tracer IO (LogObject Text)
                      -> Tracer IO (WithSeverity (WithTip blk (ChainDB.TraceEvent blk)))
    teeTraceChainTip' tr =
        Tracer $ \(WithSeverity _ (WithTip _tip ev')) ->
          case ev' of
              (ChainDB.TraceAddBlockEvent ev) -> case ev of
                  ChainDB.SwitchedToChain _ c -> do
                      meta <- mkLOMeta Critical Confidential
                      let tr' = appendName "metrics" tr
                          ChainInformation { slots, blocks, density } = chainInformation c
                          epochSlots :: Word64 = 21600  -- TODO
                      traceNamedObject tr' (meta, LogValue "density" . PureD $ fromRational density)
                      traceNamedObject tr' (meta, LogValue "slotNum" . PureI $ fromIntegral slots)
                      traceNamedObject tr' (meta, LogValue "blockNum" . PureI $ fromIntegral blocks)
                      traceNamedObject tr' (meta, LogValue "slotInEpoch" . PureI $ fromIntegral (slots `rem` epochSlots))
                      traceNamedObject tr' (meta, LogValue "epoch" . PureI $ fromIntegral (slots `div` epochSlots))
                  _ -> pure ()
              _ -> pure ()
    teeTraceBlockFetchDecision :: TracingFormatting
        -> TracingVerbosity
        -> Tracer IO (LogObject Text)
        -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision tform tverb tr = Tracer $ \ev -> do
      traceWith (teeTraceBlockFetchDecision' tr) ev
      traceWith (toLogObject' tform tverb tr) ev
    teeTraceBlockFetchDecision' :: Tracer IO (LogObject Text)
                -> Tracer IO (WithSeverity [TraceLabelPeer peer (FetchDecision [Point (Header blk)])])
    teeTraceBlockFetchDecision' tr =
        Tracer $ \(WithSeverity _ peers) -> do
          meta <- mkLOMeta Notice Confidential
          let tr' = appendName "peers" tr
          traceNamedObject tr' (meta, LogValue "connectedPeers" . PureI $ fromIntegral $ length peers)

    mempoolTraceTransformer :: Tracer IO (LogObject a)
                            -> Tracer IO (TraceEventMempool blk)
    mempoolTraceTransformer tr = Tracer $ \mempoolEvent -> do
        let tr' = appendName "metrics" tr
            (n, tot) = case mempoolEvent of
                  TraceMempoolAddTxs      txs0 tot0 _ -> (length txs0, tot0)
                  TraceMempoolRejectedTxs txs0 tot0 _ -> (length txs0, tot0)
                  TraceMempoolRemoveTxs   txs0 tot0  _-> (length txs0, tot0)
                  TraceMempoolManuallyRemovedTxs txs0 txs1 tot0
                                                    -> ( length txs0 + length txs1
                                                       , tot0
                                                       )
            logValue1 :: LOContent a
            logValue1 = LogValue "txsInMempool" $ PureI $ fromIntegral (msNumTxs tot)

            logValue2 :: LOContent a
            logValue2 = LogValue "txsProcessed" $ PureI $ fromIntegral n

        meta <- mkLOMeta Critical Confidential

        traceNamedObject tr (meta, logValue1)
        traceNamedObject tr' (meta, logValue1)

        traceNamedObject tr (meta, logValue2)
        traceNamedObject tr' (meta, logValue2)


    mempoolTracer :: Tracer IO (TraceEventMempool blk)
    mempoolTracer = Tracer $ \ev -> do
      traceWith (mempoolTraceTransformer tracer) ev
      traceWith (measureTxsStart tracer) ev
      traceWith (showTracing $ withName "Mempool" tracer) ev

    forgeTracer :: ForgeTracers -> TraceOptions -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
    forgeTracer forgeTracers traceOpts = Tracer $ \ev -> do
        traceWith (measureTxsEnd tracer) ev
        traceWith (consensusForgeTracer) ev
      where
        -- The consensus tracer.
        consensusForgeTracer = tracerOnOff (traceForge traceOpts)
          $ annotateSeverity
          $ teeForge forgeTracers StructuredLogging tracingVerbosity
          $ addName "Forge" tracer

    teeForge
      :: ForgeTracers
      -> TracingFormatting
      -> TracingVerbosity
      -> Trace IO Text
      -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk (GenTx blk)))
    teeForge ft tform tverb tr = Tracer $ \ev -> do
      traceWith (teeForge' tr) ev
      flip traceWith ev $ fanning $ \(WithSeverity _ e) ->
        case e of
          Consensus.TraceForgeEvent{} -> teeForge' (ftForged ft)
          Consensus.TraceCouldNotForge{} -> teeForge' (ftCouldNotForge ft)
          Consensus.TraceAdoptedBlock{} -> teeForge' (ftAdopted ft)
          Consensus.TraceDidntAdoptBlock{} -> teeForge' (ftDidntAdoptBlock ft)
          Consensus.TraceForgedInvalidBlock{} -> teeForge' (ftForgedInvalid ft)
      traceWith (toLogObject' tform tverb tr) ev

    teeForge'
      :: Trace IO Text
      -> Tracer IO (WithSeverity (Consensus.TraceForgeEvent blk (GenTx blk)))
    teeForge' tr =
      Tracer $ \(WithSeverity _ ev) -> do
        meta <- mkLOMeta Critical Confidential
        traceNamedObject (appendName "metrics" tr) . (meta,) $
          case ev of
            Consensus.TraceForgeEvent    slot _ ->
              LogValue "forgedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceCouldNotForge slot _ ->
              LogValue "couldNotForgeSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceAdoptedBlock slot _ _ _ ->
              LogValue "adoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceDidntAdoptBlock slot _ ->
              LogValue "notAdoptedSlotLast" $ PureI $ fromIntegral $ unSlotNo slot
            Consensus.TraceForgedInvalidBlock slot _ _ ->
              LogValue "forgedInvalidSlotLast" $ PureI $ fromIntegral $ unSlotNo slot

    mkConsensusTracers :: ForgeTracers -> TraceOptions -> Consensus.Tracers' peer blk (Tracer IO)
    mkConsensusTracers forgeTracers traceOpts = Consensus.Tracers
      { Consensus.chainSyncClientTracer
        = tracerOnOff (traceChainSyncClient traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ChainSyncClient" tracer
      , Consensus.chainSyncServerHeaderTracer
        =  tracerOnOff (traceChainSyncHeaderServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ChainSyncHeaderServer" tracer
      , Consensus.chainSyncServerBlockTracer
        = tracerOnOff (traceChainSyncBlockServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "ChainSyncBlockServer" tracer
      , Consensus.blockFetchDecisionTracer
        = tracerOnOff (traceBlockFetchDecisions traceOpts)
          $ annotateSeverity
          $ teeTraceBlockFetchDecision StructuredLogging tracingVerbosity
          $ addName "BlockFetchDecision" tracer
      , Consensus.blockFetchClientTracer
        = tracerOnOff (traceBlockFetchClient traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "BlockFetchClient" tracer
      , Consensus.blockFetchServerTracer
        = tracerOnOff (traceBlockFetchServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "BlockFetchServer" tracer
      , Consensus.txInboundTracer
        = tracerOnOff (traceTxInbound traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "TxInbound" tracer
      , Consensus.txOutboundTracer
        = tracerOnOff (traceTxOutbound traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "TxOutbound" tracer
      , Consensus.localTxSubmissionServerTracer
        = tracerOnOff (traceLocalTxSubmissionServer traceOpts)
          $ toLogObject' StructuredLogging tracingVerbosity
          $ addName "LocalTxSubmissionServer" tracer
      , Consensus.mempoolTracer
        = tracerOnOff (traceMempool traceOpts) $ mempoolTracer
      , Consensus.forgeTracer
        = forgeTracer forgeTracers traceOpts
      , Consensus.blockchainTimeTracer
        = Tracer $ \ev ->
            traceWith (toLogObject tracer) (readableTraceBlockchainTimeEvent ev)
      }

    readableTraceBlockchainTimeEvent :: TraceBlockchainTimeEvent -> Text
    readableTraceBlockchainTimeEvent ev = case ev of
        TraceStartTimeInTheFuture (SystemStart start) toWait ->
          "Waiting " <> show toWait <> " until genesis start time at " <> show start

    mkProtocolsTracers :: TraceOptions -> ProtocolTracers' peer blk DeserialiseFailure (Tracer IO)
    mkProtocolsTracers traceOpts = ProtocolTracers
      { ptChainSyncTracer
        = tracerOnOff (traceChainSyncProtocol traceOpts)
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , ptChainSyncSerialisedTracer
        = tracerOnOff (traceChainSyncProtocol traceOpts)
        $ showTracing $ withName "ChainSyncProtocol" tracer
      , ptBlockFetchTracer
        = tracerOnOff (traceBlockFetchProtocol traceOpts)
        $ showTracing $ withName "BlockFetchProtocol" tracer
      , ptBlockFetchSerialisedTracer
        = tracerOnOff (traceBlockFetchProtocolSerialised traceOpts)
        $ showTracing $ withName "BlockFetchProtocol" tracer
      , ptTxSubmissionTracer
        = tracerOnOff (traceTxSubmissionProtocol traceOpts)
        $ showTracing $ withName "TxSubmissionProtocol" tracer
      , ptLocalChainSyncTracer
        = tracerOnOff (traceLocalChainSyncProtocol traceOpts)
        $ showTracing $ withName "LocalChainSyncProtocol" tracer
      , ptLocalTxSubmissionTracer
        = tracerOnOff (traceLocalTxSubmissionProtocol traceOpts)
        $ showTracing $ withName "LocalTxSubmissionProtocol" tracer
      }

chainInformation :: forall block . AF.HasHeader block
                 => AF.AnchoredFragment block -> ChainInformation
chainInformation frag = ChainInformation
    { slots     = slotN
    , blocks    = blockN
    , density   = calcDensity blockD slotD
    }
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromMaybe 1 (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _ -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b

withName :: String
         -> Tracer IO (LogObject Text)
         -> Tracer IO String
withName name tr = contramap pack $ toLogObject $ appendName (pack name) tr

-- | Transformer for the start of the transaction, when the transaction was added
-- to the mempool.
measureTxsStart :: Tracer IO (LogObject Text) -> Tracer IO (TraceEventMempool blk)
measureTxsStart tracer = measureTxsStartInter $ toLogObject tracer
  where
    measureTxsStartInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (TraceEventMempool blk)
    measureTxsStartInter tracer' = Tracer $ \case
        TraceMempoolAddTxs txs MempoolSize{msNumTxs,msNumBytes} time ->
            traceWith tracer' measureTxsEvent
          where
            measureTxsEvent = MeasureTxsTimeStart
                                txs
                                (fromIntegral msNumTxs)
                                (fromIntegral msNumBytes)
                                time

        _ -> pure ()

-- | Transformer for the end of the transaction, when the transaction was added to the
-- block and the block was forged.
measureTxsEnd :: Tracer IO (LogObject Text) -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
measureTxsEnd tracer = measureTxsEndInter $ toLogObject tracer
  where
    measureTxsEndInter :: Tracer IO (MeasureTxs blk) -> Tracer IO (Consensus.TraceForgeEvent blk (GenTx blk))
    measureTxsEndInter tracer' = Tracer $ \case
        Consensus.TraceAdoptedBlock slotNo blk txs time   -> traceWith tracer' (MeasureTxsTimeStop slotNo blk txs time)
        _                                       -> pure ()

-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
--
withTip :: TVar IO (Point blk)
        -> Tracer IO (WithTip blk a)
        -> Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
    tip <- atomically $ readTVar varTip
    traceWith (contramap (WithTip tip) tr) msg
