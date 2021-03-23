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

import Cardano.BM.Configuration.Model
import Cardano.BM.Data.Configuration
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

loggingFeatures :: Representation -> Bool -> IO (LoggingLayer, [CardanoFeature])
loggingFeatures _ False = return (LoggingLayer Trace.nullTracer, [])
loggingFeatures rep True = do
  (loggingLayer, logging) <- loggingFeatureWithRepresentation rep
  return (loggingLayer, [logging])

loggingFeatureWithRepresentation :: Representation -> IO (LoggingLayer, CardanoFeature)
loggingFeatureWithRepresentation rep = do
  config <- setupFromRepresentation rep
  (baseTrace, switchboard) <- setupTrace_ config "morpho-checkpoint"
  let loggingLayer = LoggingLayer $ Trace.natTrace liftIO baseTrace
      feature =
        CardanoFeature
          { featureName = "Logging",
            featureStart = return (),
            featureShutdown = liftIO $ shutdown switchboard
          }
  return (loggingLayer, feature)
