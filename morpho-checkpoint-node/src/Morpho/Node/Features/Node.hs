{-# LANGUAGE RankNTypes #-}

module Morpho.Node.Features.Node (run) where

import Cardano.Prelude
import Cardano.Shell.Lib (CardanoApplication (..), runCardanoApplicationWithFeatures)
import Morpho.Config.Logging (createLoggingFeature)
import Morpho.Config.Types
import Morpho.Node.Run (runNode)

run :: NodeCLI -> IO ()
run cli = do
  nodeConfig <- parseNodeConfiguration $ unConfigPath (configFp cli)
  (loggingLayer, loggingFeature) <- createLoggingFeature cli nodeConfig
  runCardanoApplicationWithFeatures [loggingFeature] $
    CardanoApplication $ runNode loggingLayer nodeConfig cli
