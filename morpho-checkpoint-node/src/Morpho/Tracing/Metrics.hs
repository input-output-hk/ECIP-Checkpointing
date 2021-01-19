{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Morpho.Tracing.Metrics
  ( MorphoMetrics (..),
    setupPrometheus,
    setTimeDiff,
  )
where

import Cardano.BM.Counters.Common
import Cardano.BM.Data.Aggregated
import Cardano.BM.Data.Counter
import Cardano.Prelude hiding (atomically)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Class.MonadSTM.Strict
import Data.Time
import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Metric.Gauge hiding (sample)
import System.Metrics.Prometheus.Registry (RegistrySample)

data MorphoMetrics = MorphoMetrics
  { mLatestPowBlock :: Gauge,
    mMorphoStateUnstableCheckpoint :: Gauge,
    mMorphoStateStableCheckpoint :: Gauge,
    mMorphoBlockTime :: Gauge,
    mMorphoBlockNumber :: Gauge,
    mPushedCheckpoint :: Gauge,
    mNbVotesLastCheckpoint :: Gauge,
    mNbPeers :: Gauge
  }

setupPrometheus :: IO (MorphoMetrics, IO RegistrySample)
setupPrometheus = runRegistryT $ do
  mLatestPowBlock <- registerGauge "morpho_latest_pow_block_number" mempty
  mMorphoStateStableCheckpoint <- registerGauge "morpho_checkpoint_stable_state_pow_block_number" mempty
  mMorphoStateUnstableCheckpoint <- registerGauge "morpho_checkpoint_unstable_state_pow_block_number" mempty
  mPushedCheckpoint <- registerGauge "morpho_checkpoint_pushed_pow_block_number" mempty
  mMorphoBlockTime <- registerGauge "morpho_block_time" mempty
  mMorphoBlockNumber <- registerGauge "morpho_block_number" mempty
  mNbVotesLastCheckpoint <- registerGauge "morpho_checkpoint_nb_votes_latest" mempty
  mNbPeers <- registerGauge "morpho_checkpoint_nb_peers" mempty
  mLiveBytes <- registerGauge "morpho_live_bytes" mempty
  liftIO $ startMemoryCapturing mLiveBytes
  rs <- sample
  pure
    ( MorphoMetrics
        { mLatestPowBlock,
          mMorphoStateUnstableCheckpoint,
          mMorphoStateStableCheckpoint,
          mMorphoBlockTime,
          mMorphoBlockNumber,
          mPushedCheckpoint,
          mNbVotesLastCheckpoint,
          mNbPeers
        },
      rs
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
