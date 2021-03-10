{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Morpho.Tracing.Metrics
  ( Metric (..),
    prometheusMetrics,
  )
where

import Cardano.BM.Counters.Common
import Cardano.BM.Data.Aggregated
import Cardano.BM.Data.Counter
import Cardano.BM.Data.Tracer
import Cardano.Prelude hiding (atomically)
import Cardano.Shell.Types
import Cardano.Slotting.Slot
import qualified Control.Concurrent.Async as Async
import Control.Monad.Class.MonadSTM.Strict
import Data.Time
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Ouroboros.Network.Block
import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Http.Scrape (serveMetrics)
import System.Metrics.Prometheus.Metric.Gauge hiding (sample)
import Prelude (String)

data Metric h c
  = MetricLatestPowBlock PowBlockRef
  | MetricUnstableCheckpoint Checkpoint
  | MetricStableCheckpoint Checkpoint
  | MetricNewTip (Tip (MorphoBlock h c))
  | MetricPushedCheckpoint Checkpoint
  | MetricPeerCount Int

data Gauges = Gauges
  { mLatestPowBlock :: Gauge,
    mMorphoStateUnstableCheckpoint :: Gauge,
    mMorphoStateStableCheckpoint :: Gauge,
    mMorphoBlockTime :: Gauge,
    mMorphoBlockNumber :: Gauge,
    mPushedCheckpoint :: Gauge,
    mNbVotesLastCheckpoint :: Gauge,
    mNbPeers :: Gauge,
    mLiveBytes :: Gauge
  }

traceMetric :: StrictTVar IO (Maybe UTCTime) -> Gauges -> Metric h c -> IO ()
traceMetric _ gauges (MetricLatestPowBlock powBlockRef) =
  set (fromIntegral $ powBlockNo powBlockRef) $ mLatestPowBlock gauges
traceMetric _ gauges (MetricUnstableCheckpoint chkp) =
  set (fromIntegral . powBlockNo . checkpointedBlock $ chkp) $ mMorphoStateUnstableCheckpoint gauges
traceMetric _ gauges (MetricStableCheckpoint chkp) =
  set (fromIntegral . powBlockNo . checkpointedBlock $ chkp) $ mMorphoStateStableCheckpoint gauges
traceMetric lastBlockTsVar gauges (MetricNewTip tip) = do
  setTimeDiff lastBlockTsVar (mMorphoBlockTime gauges)
  let mb = withOriginToMaybe $ getTipBlockNo tip
  set (maybe 0 (realToFrac . unBlockNo) mb) $ mMorphoBlockNumber gauges
traceMetric _ gauges (MetricPushedCheckpoint chkp) = do
  set (fromIntegral . powBlockNo . checkpointedBlock $ chkp) $ mPushedCheckpoint gauges
  set (fromIntegral . length . chkpSignatures $ chkp) $ mNbVotesLastCheckpoint gauges
traceMetric _ gauges (MetricPeerCount count) =
  set (fromIntegral count) $ mNbPeers gauges

registerGauges :: RegistryT IO Gauges
registerGauges =
  Gauges
    <$> registerGauge "morpho_latest_pow_block_number" mempty
    <*> registerGauge "morpho_checkpoint_unstable_state_pow_block_number" mempty
    <*> registerGauge "morpho_checkpoint_stable_state_pow_block_number" mempty
    <*> registerGauge "morpho_block_time" mempty
    <*> registerGauge "morpho_block_number" mempty
    <*> registerGauge "morpho_checkpoint_pushed_pow_block_number" mempty
    <*> registerGauge "morpho_checkpoint_nb_votes_latest" mempty
    <*> registerGauge "morpho_checkpoint_nb_peers" mempty
    <*> registerGauge "morpho_live_bytes" mempty

prometheusMetrics :: Tracer IO String -> Int -> IO (Tracer IO (Metric h c), [CardanoFeature])
prometheusMetrics mainTracer port = runRegistryT $ do
  gauges <- registerGauges
  rs <- sample
  lastBlockTsVar <- lift $ newTVarIO Nothing
  return
    ( Tracer (traceMetric lastBlockTsVar gauges),
      [ CardanoFeature
          { featureName = "PrometheusServer",
            featureStart =
              liftIO $
                serveMetrics port ["metrics"] rs
                  `catch` (\e -> traceWith mainTracer $ show (e :: IOException)),
            featureShutdown = return ()
          },
        CardanoFeature
          { featureName = "MetricCollector",
            featureStart = liftIO $ startMemoryCapturing (mLiveBytes gauges),
            featureShutdown = return ()
          }
      ]
    )

startMemoryCapturing :: Gauge -> IO ()
startMemoryCapturing gauge = void $
  Async.async $
    forever $
      do
        threadDelay 1000000 -- 1 second
        cts <- readRTSStats
        traceMemory cts
  where
    traceMemory :: [Counter] -> IO ()
    traceMemory [] = pure ()
    traceMemory (Counter _ "gcLiveBytes" (Bytes bytes) : _) =
      set (fromIntegral bytes) gauge
    traceMemory (_ : cs) = traceMemory cs

setTimeDiff :: StrictTVar IO (Maybe UTCTime) -> Gauge -> IO ()
setTimeDiff lastBlockTsVar gauge = do
  cts <- getCurrentTime
  mlastBlockTs <- atomically $ do
    result <- readTVar lastBlockTsVar
    writeTVar lastBlockTsVar (Just cts)
    return result
  case mlastBlockTs of
    Nothing -> return ()
    Just lastBlockTs ->
      set (realToFrac $ cts `diffUTCTime` lastBlockTs) gauge
