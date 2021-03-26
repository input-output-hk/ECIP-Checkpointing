{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Morpho.Node.Run
  ( run,
  )
where

import Cardano.BM.Data.Tracer
import Cardano.Crypto.Hash
import Cardano.Prelude hiding (atomically, take, trace, traceId, unlines)
import Control.Monad.Class.MonadSTM.Strict (MonadSTM (atomically), newTVar, readTVar)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import Data.Map.Strict (size)
import Data.Text (pack)
import Morpho.Common.Socket
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.State
import Morpho.Ledger.Tx
import Morpho.Ledger.Update
import Morpho.Node.Env
import Morpho.Node.ProtocolInfo
import Morpho.Node.RunNode ()
import Morpho.RPC.Request
import Morpho.RPC.Types
import Morpho.Tracing.Metrics
import Morpho.Tracing.Tracers
import Morpho.Tracing.TracingOrphanInstances
import Morpho.Tracing.Types
import Network.DNS.Utils (normalize)
import Network.HTTP.Client hiding (Proxy)
import Network.Socket
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Node hiding (Tracers, cfg, chainDB, registry, run, tracers)
import qualified Ouroboros.Consensus.Node as Node (runWith)
import Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import Ouroboros.Consensus.Util.ResourceRegistry
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.Block
import Ouroboros.Network.NodeToClient
import Ouroboros.Network.NodeToNode hiding (RemoteAddress)
import System.Metrics.Prometheus.Http.Scrape (serveMetrics)
import System.Metrics.Prometheus.Metric.Gauge
import Prelude (id)

run :: Env MorphoMockHash ConsensusMockCrypto -> IO ()
run env = handleSimpleNode (protocolInfoMorpho env) (eTracers env) env

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode ::
  forall blk h c.
  (RunNode blk, blk ~ MorphoBlock h c, MorphoStateDefaultConstraints h c) =>
  ProtocolInfo IO blk ->
  Tracers RemoteConnectionId LocalConnectionId h c ->
  Env h c ->
  IO ()
handleSimpleNode pInfo nodeTracers env = do
  let NetworkTopology nodeSetups = eTopology env
  producers' <- case List.lookup nid $
    map (\ns -> (CoreNodeId $ nodeId ns, producers ns)) nodeSetups of
    Just ps -> do
      traceWith (morphoInitTracer nodeTracers) $ ProducerList (eNodeAddress env) nid ps
      return ps
    Nothing -> do
      traceWith (morphoInitTracer nodeTracers) $ NotFoundInTopology (eNodeAddress env) nid
      exitFailure
  -- Socket directory TODO
  addresses <- nodeAddressInfo (eNodeAddress env)
  let ipv4Address = find ((== AF_INET) . addrFamily) addresses
      ipv6Address = find ((== AF_INET6) . addrFamily) addresses
      localSocketPath = eSocketFile env
  removeStaleLocalSocket localSocketPath
  let ipProducerAddrs :: [NodeAddress]
      dnsProducerAddrs :: [RemoteAddress]
      (ipProducerAddrs, dnsProducerAddrs) =
        partitionEithers
          [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
            | ra <- producers'
          ]
      ipProducers :: IPSubscriptionTarget
      ipProducers =
        let ips = nodeAddressToSockAddr <$> ipProducerAddrs
         in IPSubscriptionTarget
              { ispIps = ips,
                ispValency = length ips
              }
      dnsProducers :: [DnsSubscriptionTarget]
      dnsProducers = producerSubscription <$> dnsProducerAddrs
      producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
      producerSubscription ra =
        DnsSubscriptionTarget
          { dstDomain = normalize (BSC.pack (raAddress ra)),
            dstPort = raPort ra,
            dstValency = raValency ra
          }
      diffusionTracers :: DiffusionTracers
      diffusionTracers =
        DiffusionTracers
          { dtIpSubscriptionTracer = ipSubscriptionTracer nodeTracers,
            dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers,
            dtDnsResolverTracer = dnsResolverTracer nodeTracers,
            dtErrorPolicyTracer = errorPolicyTracer nodeTracers,
            dtMuxTracer = muxTracer nodeTracers,
            dtMuxLocalTracer = muxLocalTracer nodeTracers,
            dtHandshakeTracer = handshakeTracer nodeTracers,
            dtHandshakeLocalTracer = handshakeLocalTracer nodeTracers,
            dtLocalErrorPolicyTracer = localErrorPolicyTracer nodeTracers,
            dtAcceptPolicyTracer = acceptPolicyTracer nodeTracers,
            dtDiffusionInitializationTracer = diffusionInitializationTracer nodeTracers,
            dtLedgerPeersTracer = ledgerPeersTracer nodeTracers
          }
      diffusionArguments :: DiffusionArguments
      diffusionArguments =
        DiffusionArguments
          { daIPv4Address = Right <$> ipv4Address,
            daIPv6Address = Right <$> ipv6Address,
            daLocalAddress = Right localSocketPath,
            daIpProducers = ipProducers,
            daDnsProducers = dnsProducers,
            -- TODO probably we could use smaller values here.
            daAcceptedConnectionsLimit =
              AcceptedConnectionsLimit
                { acceptedConnectionsHardLimit = 512,
                  acceptedConnectionsSoftLimit = 384,
                  acceptedConnectionsDelay = 5
                },
            daDiffusionMode = InitiatorAndResponderDiffusionMode
          }
  when (eValidateDb env) $
    traceWith (morphoInitTracer nodeTracers) PerformingDBValidation
  (metrics, irs) <- setupPrometheus
  let kernelHook ::
        ResourceRegistry IO ->
        NodeKernel IO RemoteConnectionId LocalConnectionId blk ->
        IO ()
      kernelHook registry nodeKernel = do
        -- Start the prometheus HTTP server
        void $
          forkLinkedThread registry "PrometheusMetrics" $
            catch
              (serveMetrics (ePrometheusPort env) ["metrics"] irs)
              (traceWith (morphoInitTracer nodeTracers) . PrometheusException)
        -- Watch the tip of the chain and store it in @varTip@ so we can include
        -- it in trace messages.
        let chainDB = getChainDB nodeKernel
        lastBlockTsVar <- atomically (newTVar Nothing)
        void $
          forkLinkedWatcher
            registry
            "WriteTip"
            Watcher
              { wFingerprint = id,
                wInitial = Nothing,
                wReader = ChainDB.getTipPoint chainDB,
                wNotify = \tip -> do
                  traceWith (chainTipTracer nodeTracers) (pack $ showPoint NormalVerbosity tip)
                  setTimeDiff lastBlockTsVar (mMorphoBlockTime metrics)
              }
        --  Track current block number
        void $
          forkLinkedWatcher
            registry
            "TrackBlockNumberMetric"
            Watcher
              { wFingerprint = id,
                wInitial = Nothing,
                wReader = ChainDB.getTipBlockNo chainDB,
                wNotify =
                  \ob -> do
                    let mb = withOriginToMaybe ob
                    set (maybe 0 blockNoToDouble mb) $ mMorphoBlockNumber metrics
              }
        --  Check if we need to push a checkpoint to the PoW node
        void $
          forkLinkedWatcher
            registry
            "PublishStableCheckpoint"
            Watcher
              { wFingerprint = id,
                wInitial = Nothing,
                wReader = ledgerState <$> ChainDB.getCurrentLedger chainDB,
                wNotify =
                  publishStableCheckpoint env nodeTracers metrics chainDB
              }
        -- Fetch the current stable PoW block
        void $ forkLinkedThread registry "RequestCurrentBlock" $ requestCurrentBlock (powNodeRpcTracer nodeTracers) nodeKernel env nodeTracers metrics
        -- Track the nb of connected peers
        void $
          forkLinkedWatcher
            registry
            "TrackNbPeersMetric"
            Watcher
              { wFingerprint = id,
                wInitial = Nothing,
                wReader = size <$> readTVar (getNodeCandidates nodeKernel),
                wNotify =
                  \nbPeers -> set (fromIntegral nbPeers) $ mNbPeers metrics
              }
  let args =
        RunNodeArgs
          { rnTraceConsensus = consensusTracers nodeTracers,
            rnTraceNTN = nodeToNodeTracers nodeTracers,
            rnTraceNTC = nodeToClientTracers nodeTracers,
            rnProtocolInfo = pInfo,
            rnNodeKernelHook = kernelHook
          }
      stdRunNodeArgs =
        StdRunNodeArgs
          { srnBfcMaxConcurrencyBulkSync = Nothing,
            srnBfcMaxConcurrencyDeadline = Nothing,
            srcChainDbValidateOverride = eValidateDb env,
            srnDatabasePath = eDatabaseDir env,
            srnDiffusionArguments = diffusionArguments,
            srnDiffusionTracers = diffusionTracers,
            srnTraceChainDB = chainDBTracer nodeTracers
          }
  lowLevelArgs <- stdLowLevelRunNodeArgsIO args stdRunNodeArgs
  let customizedLowLevelArgs =
        lowLevelArgs
          { llrnCustomiseChainDbArgs = customiseChainDbArgs
          }

  Node.runWith args customizedLowLevelArgs
  where
    blockNoToDouble = realToFrac . unBlockNo
    nid = eNodeId env
    customiseChainDbArgs cdbArgs =
      cdbArgs
        { cdbDiskPolicy =
            DiskPolicy
              { onDiskNumSnapshots = eSnapshotsOnDisk env,
                onDiskShouldTakeSnapshot = const (== eSnapshotInterval env)
              }
        }

requestCurrentBlock ::
  forall peer localPeer h c.
  MorphoStateDefaultConstraints h c =>
  Tracer IO PoWNodeRpcTrace ->
  NodeKernel IO peer localPeer (MorphoBlock h c) ->
  Env h c ->
  Tracers peer localPeer h c ->
  MorphoMetrics ->
  IO ()
requestCurrentBlock tr kernel nc nodeTracers metrics = forever $ do
  threadDelay (ePoWBlockFetchInterval nc)
  st <- atomically $ morphoLedgerState . ledgerState <$> ChainDB.getCurrentLedger chainDB
  let lastCheckpointedBlock = checkpointedBlock $ lastCheckpoint st
  handle (httpExceptionHandler FetchLatestPoWBlock $ powNodeRpcTracer nodeTracers) $ do
    er <- getLatestPoWBlock (ePoWNodeRpcUrl nc) (eCheckpointingInterval nc) lastCheckpointedBlock
    either
      (traceWith tr . RpcResponseParseError FetchLatestPoWBlock)
      processResponse
      er
  where
    processResponse :: PoWNodeRPCResponse LatestBlockResponse -> IO ()
    processResponse PoWNodeRPCResponse {responseResult = LatestBlockResponse Nothing} =
      traceWith tr RpcNoLatestPoWBlock
    processResponse resp@PoWNodeRPCResponse {responseResult = LatestBlockResponse (Just blockRef)} = do
      set (fromIntegral . powBlockNo $ blockRef) $ mLatestPowBlock metrics
      (traceWith tr . RpcLatestPoWBlock) resp
      st <- atomically $ morphoLedgerState . ledgerState <$> ChainDB.getCurrentLedger chainDB
      let maybeVote = voteBlockRef (configLedger $ getTopLevelConfig kernel) st blockRef
      case maybeVote of
        Left err ->
          traceWith (extractStateTracer nodeTracers) $ ExtractTxErrorTrace err
        Right vote ->
          tryAddTxs [voteToTx vote] >> pure ()
    voteToTx = mkMorphoGenTx . Tx
    chainDB = getChainDB kernel
    Mempool {tryAddTxs} = getMempool kernel

publishStableCheckpoint ::
  forall blk h c peer localpeer.
  (blk ~ MorphoBlock h c, RunNode blk) =>
  Env h c ->
  Tracers peer localpeer h c ->
  MorphoMetrics ->
  ChainDB.ChainDB IO blk ->
  LedgerState blk ->
  IO ()
publishStableCheckpoint env nodeTracers metrics chainDB ledgerState = do
  traceWith (extractStateTracer nodeTracers) (MorphoStateTrace $ morphoLedgerState ledgerState)
  set (ledgerStateToBlockNum ledgerState) $ mMorphoStateUnstableCheckpoint metrics
  mst <- getLatestStableLedgerState chainDB (eStableLedgerDepth env)
  case mst of
    Left err -> traceWith (timeTravelErrorTracer nodeTracers) err
    Right stableLedgerState -> do
      let morphoState = morphoLedgerState stableLedgerState
      set (ledgerStateToBlockNum stableLedgerState) $ mMorphoStateStableCheckpoint metrics
      case checkpointToPush morphoState of
        Left err -> traceWith (extractStateTracer nodeTracers) $ WontPushCheckpointTrace err
        Right chkp -> do
          traceWith (extractStateTracer nodeTracers) (PushingCheckpoint chkp) -- (MorphoStateTrace $ morphoLedgerState ledgerState)
          catch
            (pushCheckpointToPoWNode (powNodeRpcTracer nodeTracers) chkp >> updatePushedCheckpointMetrics stableLedgerState)
            (httpExceptionHandler PushCheckpoint $ powNodeRpcTracer nodeTracers)
  where
    checkpointToPush st =
      if checkpointAt st == morphoTip st && morphoTip st /= genesisPoint
        then Right $ lastCheckpoint st
        else Left $ WontPushCheckpoint (checkpointAt st) (morphoTip st)
    pushCheckpointToPoWNode tr chkp = do
      let (PowBlockHash hashBytes) = powBlockHash $ checkpointedBlock chkp
          chkpt =
            PoWBlockchainCheckpoint
              (PowBlockHash hashBytes)
              (ObftSignature . sigToHex <$> chkpSignatures chkp)
      er <- pushPoWNodeCheckpoint (ePoWNodeRpcUrl env) chkpt
      either
        (traceWith tr . RpcResponseParseError PushCheckpoint)
        (traceWith tr . RpcPushedCheckpoint)
        er
    updatePushedCheckpointMetrics st = do
      set (ledgerStateToBlockNum st) $ mPushedCheckpoint metrics
      set (ledgerStateToNbVotes st) $ mNbVotesLastCheckpoint metrics
    ledgerStateToBlockNum = fromIntegral . powBlockNo . checkpointedBlock . lastCheckpoint . morphoLedgerState
    ledgerStateToNbVotes = fromIntegral . length . chkpSignatures . lastCheckpoint . morphoLedgerState

httpExceptionHandler :: PoWNodeRpcOperation -> Tracer IO PoWNodeRpcTrace -> HttpException -> IO ()
httpExceptionHandler op t he = traceWith t . RpcNetworkError op $ displayException he

instance MorphoStateDefaultConstraints ShortHash ConsensusMockCrypto
