{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NamedFieldPuns      #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Blockchain.Node.Run
  ( runNode
  ) where

import Cardano.Prelude hiding (take, trace, traceId)
import Prelude (error, id, unlines)


import Cardano.BM.Data.Tracer
import Cardano.BM.Tracing
import Control.Monad.Class.MonadSTM (newTVar, writeTVar)
import Data.Text (Text, pack, breakOn, take)
import Network.HTTP.Client hiding (Proxy)
import Network.HostName (getHostName)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Mempool.API hiding (txId)
import Ouroboros.Consensus.Node hiding (Tracers, tracers, cfg, registry, chainDB)
import Ouroboros.Consensus.Node.Run (RunNode)
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Util.ResourceRegistry
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.Block
import Ouroboros.Storage.FS.API.Types
import Ouroboros.Storage.FS.IO
import Ouroboros.Storage.ImmutableDB.Types
import Ouroboros.Storage.LedgerDB.DiskPolicy
import System.Directory
import System.FilePath ((</>))
import System.Metrics.Prometheus.Http.Scrape (serveHttpTextMetrics)
import System.Metrics.Prometheus.Metric.Gauge
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Ouroboros.Consensus.Node as Node (run)
import qualified Ouroboros.Storage.ChainDB as ChainDB

import Blockchain.Common.LocalSocket
import Blockchain.Common.Conversions
import Blockchain.Config.Logging hiding (hostname)
import Blockchain.Config.Protocol
import Blockchain.Config.Topology
import Blockchain.Config.Types
import Blockchain.Crypto.ECDSASignature
import Blockchain.Ledger.Block
import Blockchain.Ledger.PowTypes
import Blockchain.Ledger.SnapshotTimeTravel
import Blockchain.Ledger.State
import Blockchain.Ledger.Tx
import Blockchain.Node.ProtocolInfo
import Blockchain.Node.RunNode()
import Blockchain.RPC.Request
import Blockchain.RPC.Types
import Blockchain.Tracing.Metrics
import Blockchain.Tracing.Tracers
import Blockchain.Tracing.Types


runNode
  :: LoggingLayer
  -> NodeConfiguration
  -> NodeCLI
  -> IO ()
runNode loggingLayer nc nCli = do
    hn <- hostname
    let trace = setHostname hn $
                 llAppendName loggingLayer "node" (llBasicTrace loggingLayer :: Trace IO Text)
    pInfo <- protocolInfoBft nc mockSecurityParam
    tracers <- mkTracers (traceOpts nCli) trace
    handleSimpleNode pInfo trace tracers nCli nc
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
handleSimpleNode :: forall blk c ext. (RunNode blk,
                                      MorphoCrypto c,
                                      Typeable ext,
                                      (blk ~ MorphoBlock c ext))
                 => ProtocolInfo blk
                 -> Tracer IO (LogObject Text)
                 -> Tracers ConnectionId blk c ext
                 -> NodeCLI
                 -> NodeConfiguration
                 -> IO ()
handleSimpleNode pInfo trace nodeTracers nCli nc = do
    NetworkTopology nodeSetups <-
      either error id <$> readTopologyFile (unTopology . topFile $ mscFp nCli)

    let cfg = pInfoConfig pInfo

    let tracer = contramap pack $ toLogObject trace
    traceWith tracer $
      "System started at " <> show (nodeStartTime (Proxy @blk) cfg)

    let producers' = case List.lookup nid $
                          map (\ns -> (nodeId ns, producers ns)) nodeSetups of
          Just ps -> ps
          Nothing -> error $ "handleSimpleNode: own address "
                          <> show (nodeAddr nCli)
                          <> ", Node Id "
                          <> show nid
                          <> " not found in topology"

    traceWith tracer $ unlines
      [ "**************************************"
      , "I am Node "        <> show (nodeAddr nCli) <> " Id: " <> show nid
      , "My producers are " <> show producers'
      , "**************************************"
      ]

    -- Socket directory
    myLocalAddr <- localSocketAddrInfo
                     (Just $ ncNodeId nc)
                     (unSocket . socketFile $ mscFp nCli)
                     MkdirIfMissing

    addrs <- nodeAddressInfo $ nodeAddr nCli
    let ipProducerAddrs  :: [NodeAddress]
        dnsProducerAddrs :: [RemoteAddress]
        (ipProducerAddrs, dnsProducerAddrs) = partitionEithers
          [ maybe (Right ra) Left $ remoteAddressToNodeAddress ra
          | ra <- producers' ]

        ipProducers :: IPSubscriptionTarget
        ipProducers =
          let ips = nodeAddressToSockAddr <$> ipProducerAddrs
          in IPSubscriptionTarget {
                ispIps     = ips,
                ispValency = length ips
              }

        dnsProducers :: [DnsSubscriptionTarget]
        dnsProducers = producerSubscription <$> dnsProducerAddrs

        producerSubscription :: RemoteAddress -> DnsSubscriptionTarget
        producerSubscription ra =
          DnsSubscriptionTarget
          { dstDomain  = BSC.pack (raAddress ra)
          , dstPort    = raPort ra
          , dstValency = raValency ra
          }

        diffusionTracers :: DiffusionTracers
        diffusionTracers = DiffusionTracers
          { dtIpSubscriptionTracer  = ipSubscriptionTracer  nodeTracers
          , dtDnsSubscriptionTracer = dnsSubscriptionTracer nodeTracers
          , dtDnsResolverTracer     = dnsResolverTracer     nodeTracers
          , dtErrorPolicyTracer     = errorPolicyTracer     nodeTracers
          , dtMuxTracer             = muxTracer             nodeTracers
          , dtMuxLocalTracer        = nullTracer
          , dtHandshakeTracer       = nullTracer
          , dtHandshakeLocalTracer  = nullTracer
          }

        diffusionArguments :: DiffusionArguments
        diffusionArguments = DiffusionArguments
          { daAddresses             = addrs
          , daLocalAddress          = myLocalAddr
          , daIpProducers           = ipProducers
          , daDnsProducers          = dnsProducers
          }

    removeStaleLocalSocket (Just $ ncNodeId nc) (unSocket . socketFile $ mscFp nCli)

    dbPath <- canonicalizePath =<< makeAbsolute (unDB . dBFile $ mscFp nCli)

    varTip <- atomically $ newTVar GenesisPoint

    when (validateDB nCli) $
      traceWith tracer "Performing DB validation"

    (metrics, irs) <- setupPrometheus
    -- TODO: this is getting out of hand. We need to extract some
    -- functions to their proper modules here.
    Node.run
      (consensusTracers nodeTracers)
      (withTip varTip $ chainDBTracer nodeTracers)
      diffusionTracers
      diffusionArguments
      (nodeNetworkMagic (Proxy @blk) cfg)
      (dbPath <> "-" <> show nid)
      pInfo
      IsProducer
      customiseChainDbArgs
      id -- No NodeParams customisation
      $ \registry nodeKernel -> do

        -- Start the prometheus HTTP server
        void $ forkLinkedThread registry $ catch (serveHttpTextMetrics (ncPrometheusPort nc) ["metrics"] irs)
                                                 (\e -> traceWith tracer $ show (e :: IOException))

        -- Watch the tip of the chain and store it in @varTip@ so we can include
        -- it in trace messages.
        let chainDB = getChainDB nodeKernel
        void $ onEachChange registry id Nothing (ChainDB.getTipPoint chainDB) $ \tip ->
          atomically $ writeTVar varTip tip

        --  Check if we need to push a checkpoint to the blockchain node
        void $ onEachChange registry id Nothing (ledgerState <$> ChainDB.getCurrentLedger chainDB) $
          publishStableCheckpoint cfg nc nodeTracers dbPath metrics nid registry chainDB pInfo

        -- Fetch the current blockchain stable block
        void $ forkLinkedThread registry $ requestCurrentBlock (blockchainRpcTracer nodeTracers) nodeKernel nc nodeTracers metrics
  where
    nid = case ncNodeId nc of
            (CoreId  n) -> n
            (RelayId _) -> error "Non-core nodes currently not supported"
    customiseChainDbArgs args = args
      { ChainDB.cdbValidation = if validateDB nCli
          then ValidateAllEpochs
          else ValidateMostRecentEpoch
      , ChainDB.cdbDiskPolicy =  DiskPolicy {
          onDiskNumSnapshots  = fromIntegral $ ncSnapshotsOnDisk nc
        , onDiskWriteInterval = return $ fromIntegral $ ncSnapshotInterval nc
        }
      }

requestCurrentBlock :: Tracer IO BlockchainRpcTrace ->
                       NodeKernel IO peer (MorphoBlock c ext) ->
                       NodeConfiguration ->
                       Tracers peer blk c ext ->
                       MorphoMetrics ->
                       IO ()
requestCurrentBlock tr kernel nc nodeTracers metrics = forever $ do
  threadDelay (fromMaybe 1000 $ ncBlockchainBlockFetchInterval nc)
  catch
    go
    (httpExceptionHandler BlockchainFetchLatestBlock $ blockchainRpcTracer nodeTracers)
  where
    go = do
      er <- getBlockchainLatestBlock (ncBlockchainRpcUrl nc) (ncCheckpointInterval nc)
      either
        (traceWith tr . BlockchainRpcResponseParseError BlockchainFetchLatestBlock . pack)
        processResponse
        er
    processResponse :: BlockchainRPCResponse BlockchainLatestBlockResult -> IO ()
    processResponse resp = do
      set (fromIntegral . powBlockNo $ blockRef resp) $ mLatestPowBlock metrics
      st <- atomically $ morphoLedgerState . ledgerState <$> ChainDB.getCurrentLedger chainDB
      let maybeVote = considerCandidate st (blockRef resp)
      case maybeVote of
        Nothing -> pure ()
        Just vote -> addTxs [voteToTx vote] >> pure ()
      (traceWith tr . BlockchainRpcLatestBlock) resp
    blockRef (BlockchainRPCResponse _ (BlockchainLatestBlockResult n h) _) = PowBlockRef n h
    voteToTx = mkMorphoGenTx . Tx
    chainDB = getChainDB kernel
    Mempool{addTxs} = getMempool kernel


publishStableCheckpoint ::
  forall blk c ext peer.
  (blk ~ MorphoBlock c ext, MorphoCrypto c, Typeable ext, RunNode blk) =>
  NodeConfig (BlockProtocol blk) ->
  NodeConfiguration ->
  Tracers peer blk c ext ->
  FilePath ->
  MorphoMetrics ->
  Int ->
  ResourceRegistry IO ->
  ChainDB.ChainDB IO blk ->
  ProtocolInfo blk ->
  LedgerState blk ->
  IO ()
publishStableCheckpoint cfg nc nodeTracers dbPath metrics nid registry chainDB pInfo extState = do
  traceWith (morphoStateTracer nodeTracers) (MorphoStateTrace extState)

  set (ledgerStateToBlockNum extState) $ mMorphoStateUnstableCheckpoint metrics
  let dbHasFs = ioHasFS . MountPoint $ dbPath <> "-" <> show nid </> "ledger"
  -- -- ^ Note: we are opening the database again. I did not
  -- --         manage to get access to the internal
  -- --         ourobouros-network HasFs handle

      els = getLatestStableLedgerState
        dbHasFs
        (nodeDecodeLedgerState cfg)
        (decodePoint $ nodeDecodeHeaderHash (Proxy @blk))
        registry
        chainDB
        (ledgerState $ pInfoInitLedger pInfo)

  runExceptT els >>= \case
    Left err -> traceWith (timeTravelErrorTracer nodeTracers) (TimeTravelErrorTrace err)
    Right stableLedgerState -> do
      let morphoState = morphoLedgerState stableLedgerState
      set (ledgerStateToBlockNum stableLedgerState) $ mMorphoStateStableCheckpoint metrics
      case checkpointToPush morphoState of
          Nothing -> pure ()
          Just chkp -> do
            traceWith (morphoStateTracer nodeTracers) (MorphoStateTrace extState)
            catch
                (pushCheckpointToBlockchain (blockchainRpcTracer nodeTracers) chkp >> updatePushedCheckpointMetrics stableLedgerState)
                (httpExceptionHandler BlockchainPushCheckpoint $ blockchainRpcTracer nodeTracers)
  where
    checkpointToPush st =
      if checkpointAt st == morphoTip st && morphoTip st /= genesisPoint
        then Just $ lastCheckpoint st
        else Nothing
    pushCheckpointToBlockchain tr chkp = do
      let
        (PowBlockHash hashBytes) = powBlockHash $ checkpointedBlock chkp
        chkpt = BlockchainCheckpoint
                (BlockchainBlockHash $ bytesToHex hashBytes)
                (ObftSignature . sigToHex <$> chkpSignatures chkp)
      er <- pushBlockchainCheckpoint (ncBlockchainRpcUrl nc) chkpt
      either
        (traceWith tr . BlockchainRpcResponseParseError BlockchainPushCheckpoint . pack)
        (traceWith tr . BlockchainRpcPushedCheckpoint) er
    updatePushedCheckpointMetrics st = do
      set (ledgerStateToBlockNum st) $ mPushedCheckpoint metrics
      set (ledgerStateToNbVotes st) $ mNbVotesLastCheckpoint metrics
    ledgerStateToBlockNum = fromIntegral . powBlockNo . checkpointedBlock . lastCheckpoint . morphoLedgerState
    ledgerStateToNbVotes = fromIntegral . length . chkpSignatures . lastCheckpoint . morphoLedgerState


httpExceptionHandler :: BlockchainRpcOperation -> Tracer IO BlockchainRpcTrace -> HttpException -> IO ()
httpExceptionHandler op t he = traceWith t . BlockchainRpcNetworkError op . pack $ displayException he
