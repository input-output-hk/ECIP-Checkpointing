{-# LANGUAGE RankNTypes #-}

module Morpho.Node.Features.Node (run) where

import Cardano.Prelude
import Cardano.Shell.Lib (CardanoApplication (..), runCardanoApplicationWithFeatures)
import Morpho.Config.Logging (loggingFeatures)
import Morpho.Config.Types
import Morpho.Node.Run (runNode)

run :: NodeCLI -> IO ()
run cli = do
  nodeConfig <- parseNodeConfiguration $ unConfigPath (configFp cli)
  (loggingLayer, logging) <- loggingFeatures cli nodeConfig
  runCardanoApplicationWithFeatures logging $
    CardanoApplication $ runNode loggingLayer nodeConfig cli
