{-# LANGUAGE FlexibleContexts #-}
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
    runNode,
  )
where

import Cardano.BM.Data.Tracer
import Cardano.BM.Data.Transformers (setHostname)
import Cardano.BM.Tracing
import Cardano.Crypto.Hash
import Cardano.Prelude hiding (atomically, take, trace, traceId, unlines)
import Cardano.Shell.Lib (CardanoApplication (..), runCardanoApplicationWithFeatures)
import Control.Monad (fail)
import Control.Monad.Class.MonadSTM.Strict (MonadSTM (atomically), newTVar, readTVar)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (size)
import Data.Text (Text, breakOn, pack, take)
import Morpho.Common.Conversions
import Morpho.Common.Socket
import Morpho.Config.Logging (loggingFeatures)
import Morpho.Config.Logging hiding (hostname)
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.State
import Morpho.Ledger.Tx
import Morpho.Ledger.Update
import Morpho.Node.ProtocolInfo
import Morpho.Node.RunNode ()
import Morpho.RPC.Request
import Morpho.RPC.Types
import Morpho.Tracing.Metrics
import Morpho.Tracing.Tracers
import Morpho.Tracing.TracingOrphanInstances
import Morpho.Tracing.Types
import Network.HTTP.Client hiding (Proxy)
import Network.HostName (getHostName)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Fragment.InFuture (defaultClockSkew)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Node hiding (Tracers, cfg, chainDB, registry, run, tracers)
import qualified Ouroboros.Consensus.Node as Node (run)
import Ouroboros.Consensus.Node.Run (RunNode)
import Ouroboros.Consensus.NodeId
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.ImmutableDB.Types
import Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import Ouroboros.Consensus.Storage.VolatileDB
import Ouroboros.Consensus.Util.ResourceRegistry
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.Block
import Ouroboros.Network.NodeToClient
import Ouroboros.Network.Server.RateLimiting
import System.Directory
import System.Metrics.Prometheus.Http.Scrape (serveHttpTextMetrics)
import System.Metrics.Prometheus.Metric.Gauge
import Prelude (error, id, unlines)

run :: NodeCLI -> IO ()
run cli = do
  mnodeConfig <- parseNodeConfiguration $ unConfigPath (configFp cli)
  case mnodeConfig of
    Nothing -> do
      fail "Something is missing in the config"
    Just nodeConfig -> do
      (loggingLayer, logging) <- loggingFeatures cli nodeConfig
      runCardanoApplicationWithFeatures logging $
        CardanoApplication $ runNode loggingLayer nodeConfig cli

runNode ::
  LoggingLayer ->
  NodeConfiguration ->
  NodeCLI ->
  IO ()
runNode loggingLayer nc nCli = do
  hn <- hostname
  let trace =
        setHostname hn $
          appendName "node" (llBasicTrace loggingLayer)

  privKeyStr <- liftIO . readFile $ ncNodePrivKeyFile nc
  privKey <- case importPrivateKey $ bytesFromHex privKeyStr of
    Nothing -> fail $ "Invalid private key in: " <> show (ncNodePrivKeyFile nc)
    Just pk -> return pk

  let pInfo = protocolInfoMorpho nc privKey
  tracers <- mkTracers (ncTraceOpts nc) trace
  handleSimpleNode pInfo trace tracers nCli nc
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode ::
  forall blk h c.
  ( RunNode blk,
    (blk ~ MorphoBlock h c)
  ) =>
  ProtocolInfo IO blk ->
  Trace IO Text -> -- (LogObject Text)
  Tracers RemoteConnectionId LocalConnectionId h c ->
  NodeCLI ->
  NodeConfiguration ->
  IO ()
handleSimpleNode pInfo trace nodeTracers nCli nc = do
  NetworkTopology nodeSetups <-
    either error id <$> readTopologyFile (unTopology . topFile $ mscFp nCli)
  let cfg = pInfoConfig pInfo
  let tracer = contramap pack $ toLogObject trace
  let producers' = case List.lookup nid $
        map (\ns -> (CoreNodeId $ nodeId ns, producers ns)) nodeSetups of
        Just ps -> ps
        Nothing ->
          error $
            "handleSimpleNode: own address "
              <> show (nodeAddr nCli)
              <> ", Node Id "
              <> show nid
              <> " not found in topology"
  traceWith tracer $
    unlines
      [ "",
        "**************************************",
        "I am Node " <> show (nodeAddr nCli) <> " Id: " <> show nid,
        "My producers are " <> show producers',
        "**************************************"
      ]
  -- Socket directory TODO
  addresses <- nodeAddressInfo (nodeAddr nCli)
  let localSocketPath = unSocket . socketFile $ mscFp nCli
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
          { dstDomain = BSC.pack (raAddress ra),
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
            dtAcceptPolicyTracer = acceptPolicyTracer nodeTracers
          }
      diffusionArguments :: DiffusionArguments
      diffusionArguments =
        DiffusionArguments
          { daAddresses = Right addresses,
            daLocalAddress = Right localSocketPath,
            daIpProducers = ipProducers,
            daDnsProducers = dnsProducers,
            -- TODO probably we could use smaller values here.
            daAcceptedConnectionsLimit =
              AcceptedConnectionsLimit
                { acceptedConnectionsHardLimit = 512,
                  acceptedConnectionsSoftLimit = 384,
                  acceptedConnectionsDelay = 5
                }
          }
  dbPath <- canonicalizePath =<< makeAbsolute (unDB . dBFile $ mscFp nCli)
  when (validateDB nCli) $
    traceWith tracer "Performing DB validation"
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
              (serveHttpTextMetrics (ncPrometheusPort nc) ["metrics"] irs)
              (\e -> traceWith tracer $ show (e :: IOException))
        -- Watch the tip of the chain and store it in @varTip@ so we can include
        -- it in trace messages.
        let chainDB = getChainDB nodeKernel
        lastBlockTsVar <- atomically (newTVar Nothing)
        void $
          onEachChange registry "WriteTip" id Nothing (ChainDB.getTipPoint chainDB) $ \tip -> do
            traceWith (chainTipTracer nodeTracers) (pack $ showPoint NormalVerbosity tip)
            setTimeDiff lastBlockTsVar (mMorphoBlockTime metrics)
        --  Track current block number
        void $
          onEachChange registry "TrackBlockNumberMetric" id Nothing (ChainDB.getTipBlockNo chainDB) $
            \ob -> do
              let mb = withOriginToMaybe ob
              set (maybe 0 blockNoToDouble mb) $ mMorphoBlockNumber metrics
        --  Check if we need to push a checkpoint to the PoW node
        void $
          onEachChange registry "PublishStableCheckpoint" id Nothing (ledgerState <$> ChainDB.getCurrentLedger chainDB) $
            publishStableCheckpoint nc nodeTracers metrics chainDB
        -- Fetch the current stable PoW block
        void $ forkLinkedThread registry "RequestCurrentBlock" $ requestCurrentBlock (powNodeRpcTracer nodeTracers) nodeKernel nc nodeTracers metrics
        -- Track the nb of connected peers
        void $
          onEachChange registry "TrackNbPeersMetric" id Nothing (size <$> readTVar (getNodeCandidates nodeKernel)) $
            \nbPeers -> set (fromIntegral nbPeers) $ mNbPeers metrics
  let args =
        RunNodeArgs
          { rnTraceConsensus = consensusTracers nodeTracers,
            rnTraceNTN = nodeToNodeTracers nodeTracers,
            rnTraceNTC = nodeToClientTracers nodeTracers,
            rnTraceDB = chainDBTracer nodeTracers,
            rnTraceDiffusion = diffusionTracers,
            rnDiffusionArguments = diffusionArguments,
            rnNetworkMagic = getNetworkMagic $ blockConfigBlock $ topLevelConfigBlock cfg,
            rnDatabasePath = dbPath,
            rnProtocolInfo = pInfo,
            rnCustomiseChainDbArgs = customiseChainDbArgs,
            rnCustomiseNodeArgs = id,
            rnNodeToNodeVersions = NE.fromList [()],
            rnNodeToClientVersions = NE.fromList [()],
            rnNodeKernelHook = kernelHook,
            rnMaxClockSkew = defaultClockSkew
          }
  Node.run args
  where
    blockNoToDouble = realToFrac . unBlockNo
    nid = ncNodeId nc
    customiseChainDbArgs args =
      args
        { ChainDB.cdbImmValidation =
            if validateDB nCli
              then ValidateAllChunks
              else ValidateMostRecentChunk,
          ChainDB.cdbVolValidation =
            if validateDB nCli
              then ValidateAll
              else NoValidation,
          ChainDB.cdbDiskPolicy =
            DiskPolicy
              { onDiskNumSnapshots = fromIntegral $ ncSnapshotsOnDisk nc,
                onDiskShouldTakeSnapshot = const (== ncSnapshotInterval nc)
              }
        }

requestCurrentBlock ::
  forall peer localPeer h c.
  Tracer IO PoWNodeRpcTrace ->
  NodeKernel IO peer localPeer (MorphoBlock h c) ->
  NodeConfiguration ->
  Tracers peer localPeer h c ->
  MorphoMetrics ->
  IO ()
requestCurrentBlock tr kernel nc nodeTracers metrics = forever $ do
  threadDelay (ncPoWBlockFetchInterval nc)
  handle (httpExceptionHandler FetchLatestPoWBlock $ powNodeRpcTracer nodeTracers) $ do
    er <- getLatestPoWBlock (ncPoWNodeRpcUrl nc) (ncCheckpointInterval nc)
    either
      (traceWith tr . RpcResponseParseError FetchLatestPoWBlock)
      processResponse
      er
  where
    processResponse :: PoWNodeRPCResponse PowBlockRef -> IO ()
    processResponse resp = do
      let blockRef = responseResult resp
      set (fromIntegral . powBlockNo $ blockRef) $ mLatestPowBlock metrics
      (traceWith tr . RpcLatestPoWBlock) resp
      st <- atomically $ morphoLedgerState . ledgerState <$> ChainDB.getCurrentLedger chainDB
      let maybeVote = voteBlockRef (blockConfigLedger $ topLevelConfigBlock $ getTopLevelConfig kernel) st blockRef
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
  NodeConfiguration ->
  Tracers peer localpeer h c ->
  MorphoMetrics ->
  ChainDB.ChainDB IO blk ->
  LedgerState blk ->
  IO ()
publishStableCheckpoint nc nodeTracers metrics chainDB ledgerState = do
  traceWith (extractStateTracer nodeTracers) (MorphoStateTrace $ morphoLedgerState ledgerState)
  set (ledgerStateToBlockNum ledgerState) $ mMorphoStateUnstableCheckpoint metrics
  mst <- getLatestStableLedgerState chainDB (ncStableLedgerDepth nc)
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
      er <- pushPoWNodeCheckpoint (ncPoWNodeRpcUrl nc) chkpt
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
