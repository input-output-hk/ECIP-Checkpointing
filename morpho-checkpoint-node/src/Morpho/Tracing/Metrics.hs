module Morpho.Tracing.Metrics
  ( MorphoMetrics (..),
    setupPrometheus,
    UnstableBlockTimes,
    newUnstableBlockTimes,
    putUnstableTime,
    getTimeToStable,
  )
where

import Cardano.Prelude hiding (atomically, STM)
import Control.Monad.Class.MonadSTM.Strict
import Data.Time
import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Metric.Gauge hiding (sample)
import System.Metrics.Prometheus.Registry (RegistrySample)
import Morpho.Ledger.PowTypes ( PowBlockNo(PowBlockNo) )
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

data MorphoMetrics
  = MorphoMetrics
      { mLatestPowBlock :: Gauge,
        mMorphoStateUnstableCheckpoint :: Gauge,
        mMorphoStateStableCheckpoint :: Gauge,
        mMorphoTimeToStable :: Gauge,
        mPushedCheckpoint :: Gauge,
        mNbVotesLastCheckpoint :: Gauge,
        mNbPeers :: Gauge
      }

setupPrometheus :: IO (MorphoMetrics, IO RegistrySample)
setupPrometheus = runRegistryT $ do
  currentPowNumber <- registerGauge "morpho_latest_pow_block_number" mempty
  mStableStateCheckpoint <- registerGauge "morpho_checkpoint_stable_state_pow_block_number" mempty
  mUnstableStateCheckpoint <- registerGauge "morpho_checkpoint_unstable_state_pow_block_number" mempty
  mStableTime <- registerGauge "morpho_checkpoint_time_to_stable" mempty
  mpushedCheckpoint <- registerGauge "morpho_checkpoint_pushed_pow_block_number" mempty
  mNbVotes <- registerGauge "morpho_checkpoint_nb_votes_latest" mempty
  mnbp <- registerGauge "morpho_checkpoint_nb_peers" mempty
  rs <- sample
  pure (MorphoMetrics currentPowNumber mUnstableStateCheckpoint mStableStateCheckpoint mStableTime mpushedCheckpoint mNbVotes mnbp, rs)


{-
Utilities for measuring the time for an unstable checkpoint block to become stable
We store an IntMap UTCTime to track the forge/receive times of unstable blocks,
indexed by the block number on the POW chain. These timings are only correct under
the assumption that block numbers are monotonically increasing and unique.
-}

type UnstableBlockTimes = StrictTVar IO (IntMap UTCTime)

-- | Creates a new variable to store times of checkpoint blocks first appearing (still unstable)
newUnstableBlockTimes :: STM IO UnstableBlockTimes
newUnstableBlockTimes = newTVar IntMap.empty

-- | Registers a checkpoint for a specific POW block to have appeared at a specific time
-- Overrides any previous registered times for the same checkpoint
putUnstableTime :: UnstableBlockTimes -> PowBlockNo -> UTCTime -> STM IO ()
putUnstableTime times (PowBlockNo block) now = modifyTVar times (IntMap.insert block now)

-- | Returns the time it took for a checkpoint with a specific POW block to become stable
getTimeToStable :: UnstableBlockTimes -> PowBlockNo -> UTCTime -> STM IO (Maybe NominalDiffTime)
getTimeToStable times (PowBlockNo block) now = do
  -- Look up the block that just became stable, splitting the map at that point
  -- Blocks before the now-stable block will also be stable, so we don't need to keep those timings around anymore
  -- Blocks after the now-stable block are still unstable, so keep those timings around
  (_, mUnstableTime, stillUnstableTs) <- IntMap.splitLookup block <$> readTVar times
  writeTVar times stillUnstableTs
  return $ (`diffUTCTime` now) <$> mUnstableTime
