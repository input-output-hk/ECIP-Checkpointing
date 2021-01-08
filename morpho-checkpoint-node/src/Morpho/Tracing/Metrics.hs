module Morpho.Tracing.Metrics
  ( MorphoMetrics (..),
    setupPrometheus,
    setTimeDiff,
  )
where

import Cardano.Prelude hiding (atomically)
import Control.Monad.Class.MonadSTM.Strict
import Data.Time
import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Metric.Gauge hiding (sample)
import System.Metrics.Prometheus.Registry (RegistrySample)

data MorphoMetrics
  = MorphoMetrics
      { mLatestPowBlock :: Gauge,
        mMorphoStateUnstableCheckpoint :: Gauge,
        mMorphoStateStableCheckpoint :: Gauge,
        mMorphoBlockTime :: Gauge,
        mPushedCheckpoint :: Gauge,
        mNbVotesLastCheckpoint :: Gauge,
        mNbPeers :: Gauge
      }

setupPrometheus :: IO (MorphoMetrics, IO RegistrySample)
setupPrometheus = runRegistryT $ do
  currentPowNumber <- registerGauge "morpho_latest_pow_block_number" mempty
  mStableStateCheckpoint <- registerGauge "morpho_checkpoint_stable_state_pow_block_number" mempty
  mUnstableStateCheckpoint <- registerGauge "morpho_checkpoint_unstable_state_pow_block_number" mempty
  mpushedCheckpoint <- registerGauge "morpho_checkpoint_pushed_pow_block_number" mempty
  mBlockTime <- registerGauge "morpho_block_time" mempty
  mNbVotes <- registerGauge "morpho_checkpoint_nb_votes_latest" mempty
  mnbp <- registerGauge "morpho_checkpoint_nb_peers" mempty
  rs <- sample
  pure (MorphoMetrics currentPowNumber mUnstableStateCheckpoint mStableStateCheckpoint mBlockTime mpushedCheckpoint mNbVotes mnbp, rs)

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
