module Blockchain.Tracing.Metrics (
  MorphoMetrics (..),
  setupPrometheus
) where

import Cardano.Prelude

import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Registry (RegistrySample)
import System.Metrics.Prometheus.Metric.Gauge hiding (sample)

data MorphoMetrics = MorphoMetrics {
  mLatestPowBlock :: Gauge,
  mMorphoStateUnstableCheckpoint :: Gauge,
  mMorphoStateStableCheckpoint :: Gauge,
  mPushedCheckpoint :: Gauge,
  mNbVotesLastCheckpoint :: Gauge
}

setupPrometheus :: IO (MorphoMetrics, IO RegistrySample)
setupPrometheus = runRegistryT $ do
    currentPowNumber <- registerGauge "morpho_blockchain_latest_pow_block_number" mempty
    mStableStateCheckpoint <- registerGauge "morpho_checkpoint_stable_state_pow_block_number" mempty
    mUnstableStateCheckpoint <- registerGauge "morpho_checkpoint_unstable_state_pow_block_number" mempty
    mpushedCheckpoint <- registerGauge "morpho_checkpoint_pushed_pow_block_number" mempty
    mnbVotes          <- registerGauge "morpho_checkpoint_nb_votes_latest" mempty
    rs <- sample
    pure (MorphoMetrics currentPowNumber mUnstableStateCheckpoint mStableStateCheckpoint mpushedCheckpoint mnbVotes, rs)
