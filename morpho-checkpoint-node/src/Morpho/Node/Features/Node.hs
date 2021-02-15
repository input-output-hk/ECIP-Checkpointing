{-# LANGUAGE RankNTypes #-}

module Morpho.Node.Features.Node
  ( run,
    runConfig,
    NodeLayer (..),
    createNodeFeature,
  )
where

import Cardano.Prelude
import Cardano.Shell.Lib (CardanoApplication (..), runCardanoApplicationWithFeatures)
import Cardano.Shell.Types (CardanoFeature (..))
import Morpho.Config.Logging
import Morpho.Config.Types
import Morpho.Node.Run (runNode)

newtype NodeLayer = NodeLayer
  { nlRunNode :: forall m. MonadIO m => m ()
  }

createNodeFeature ::
  LoggingLayer ->
  NodeConfiguration ->
  NodeCLI ->
  IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer nc nCli = do
  let nodeLayer =
        NodeLayer
          { nlRunNode = liftIO $ runNode loggingLayer nc nCli
          }
  let cardanoFeature :: CardanoFeature
      cardanoFeature =
        CardanoFeature
          { featureName = "NodeFeature",
            featureStart = void $ pure nodeLayer,
            featureShutdown = pure ()
          }
  pure (nodeLayer, cardanoFeature)

run :: NodeCLI -> IO ()
run cli = do
  nodeConfig <- parseNodeConfiguration $ unConfigPath (configFp cli)
  runConfig cli nodeConfig

runConfig :: NodeCLI -> NodeConfiguration -> IO ()
runConfig cli nodeConfig = do
  (features, nodeLayer) <- initializeAllFeatures nodeConfig cli
  runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)
  where
    cardanoApplication :: NodeLayer -> CardanoApplication
    cardanoApplication = CardanoApplication . nlRunNode

initializeAllFeatures ::
  NodeConfiguration ->
  NodeCLI ->
  IO ([CardanoFeature], NodeLayer)
initializeAllFeatures nodeConfig nCli = do
  (loggingLayer, loggingFeature) <- createLoggingFeature nCli nodeConfig
  (nodeLayer, nodeFeature) <- createNodeFeature loggingLayer nodeConfig nCli
  pure
    ( [loggingFeature, nodeFeature] :: [CardanoFeature],
      nodeLayer
    )
