{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Morpho.Config.Logging
  ( LoggingLayer (..),
    -- re-exports
    Trace,
    Configuration,
    LoggerName,
    Severity (..),
    mkLOMeta,
    LOMeta (..),
    LOContent (..),
    loggingFeatures,
  )
where

import qualified Cardano.BM.Backend.Editor as Editor
import Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration as Config
import Cardano.BM.Counters (readCounters)
import Cardano.BM.Data.Counter
import Cardano.BM.Data.LogItem
  ( LOContent (..),
    LOMeta (..),
    LoggerName,
    PrivacyAnnotation (..),
    mkLOMeta,
  )
import Cardano.BM.Data.Observable
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Data.SubTrace
import Cardano.BM.Plugin
import Cardano.BM.Setup (setupTrace_, shutdown)
import Cardano.BM.Trace (Trace, appendName, traceNamedObject)
import qualified Cardano.BM.Trace as Trace
import Cardano.Prelude hiding (trace)
import Cardano.Shell.Types (CardanoFeature (..))
import Morpho.Config.Types

-- | The LoggingLayer interface that we can expose.
-- We want to do this since we want to be able to mock out any function tied to logging.
newtype LoggingLayer = LoggingLayer
  { llBasicTrace :: forall m. MonadIO m => Trace m Text
  }

loggingFeatures :: NodeCLI -> NodeConfiguration -> IO (LoggingLayer, [CardanoFeature])
loggingFeatures nCli nc
  | ncLoggingSwitch nc = do
    (loggingLayer, logging) <- loggingFeatureWithConfigFile $ unConfigPath $ configFp nCli
    let metrics = metricsFeature (llBasicTrace loggingLayer)
    return (loggingLayer, [logging, metrics])
  | otherwise = return (LoggingLayer Trace.nullTracer, [])

loggingFeatureWithConfigFile :: FilePath -> IO (LoggingLayer, CardanoFeature)
loggingFeatureWithConfigFile fp = do
  config <- Config.setup fp
  (baseTrace, switchboard) <- setupTrace_ config "morpho-checkpoint"
  Editor.plugin config baseTrace switchboard >>= loadPlugin switchboard
  let loggingLayer = LoggingLayer $ Trace.natTrace liftIO baseTrace
      feature =
        CardanoFeature
          { featureName = "Logging",
            featureStart = return (),
            featureShutdown = liftIO $ shutdown switchboard
          }
  return (loggingLayer, feature)

metricsFeature :: (forall m. MonadIO m => Trace m Text) -> CardanoFeature
metricsFeature trace =
  CardanoFeature
    { featureName = "Metrics",
      featureStart = startCapturingMetrics trace,
      featureShutdown = return ()
    }

startCapturingMetrics :: forall m. MonadIO m => Trace m Text -> m ()
startCapturingMetrics trace0 = do
  let trace = appendName "node-metrics" trace0
      counters = [MemoryStats, ProcessStats, NetStats, IOStats]
  forever $ do
    cts <- liftIO $ readCounters (ObservableTraceSelf counters)
    traceCounters trace cts
    liftIO $ threadDelay 30000000 -- 30 seconds
  where
    traceCounters :: Trace m a -> [Counter] -> m ()
    traceCounters _tr [] = return ()
    traceCounters tr (c@(Counter _ct cn cv) : cs) = do
      mle <- mkLOMeta Notice Confidential
      traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
      traceCounters tr cs
