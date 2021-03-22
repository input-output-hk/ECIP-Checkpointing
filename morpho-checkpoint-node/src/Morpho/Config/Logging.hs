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

import Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration as Config
import Cardano.BM.Data.LogItem
  ( LOContent (..),
    LOMeta (..),
    LoggerName,
    mkLOMeta,
  )
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Setup (setupTrace_, shutdown)
import Cardano.BM.Trace (Trace)
import qualified Cardano.BM.Trace as Trace
import Cardano.Prelude hiding (trace)
import Cardano.Shell.Types (CardanoFeature (..))

-- | The LoggingLayer interface that we can expose.
-- We want to do this since we want to be able to mock out any function tied to logging.
newtype LoggingLayer = LoggingLayer
  { llBasicTrace :: forall m. MonadIO m => Trace m Text
  }

loggingFeatures :: FilePath -> Bool -> IO (LoggingLayer, [CardanoFeature])
loggingFeatures _ False = return (LoggingLayer Trace.nullTracer, [])
loggingFeatures fp True = do
  (loggingLayer, logging) <- loggingFeatureWithConfigFile fp
  return (loggingLayer, [logging])

loggingFeatureWithConfigFile :: FilePath -> IO (LoggingLayer, CardanoFeature)
loggingFeatureWithConfigFile fp = do
  config <- Config.setup fp
  (baseTrace, switchboard) <- setupTrace_ config "morpho-checkpoint"
  let loggingLayer = LoggingLayer $ Trace.natTrace liftIO baseTrace
      feature =
        CardanoFeature
          { featureName = "Logging",
            featureStart = return (),
            featureShutdown = liftIO $ shutdown switchboard
          }
  return (loggingLayer, feature)
