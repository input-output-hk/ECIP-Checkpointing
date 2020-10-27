{-# LANGUAGE RankNTypes #-}

module Morpho.Node.Features.Node
  ( run,
    NodeLayer (..),
    createNodeFeature,
  )
where

import Cardano.Prelude
import Cardano.Shell.Types (CardanoFeature (..))
import Morpho.Config.Logging
import Morpho.Config.Types
import Morpho.Node.Run (runNode)
import Cardano.Shell.Lib (CardanoApplication (..), runCardanoApplicationWithFeatures)
import Morpho.Config.Logging (createLoggingFeature)

data NodeLayer
  = NodeLayer
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
  (features, nodeLayer) <- initializeAllFeatures cli
  runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)
  where
    cardanoApplication :: NodeLayer -> CardanoApplication
    cardanoApplication = CardanoApplication . nlRunNode

initializeAllFeatures ::
  NodeCLI ->
  IO ([CardanoFeature], NodeLayer)
initializeAllFeatures nCli@NodeCLI {configFp = ncFp} = do
  (loggingLayer, loggingFeature) <- createLoggingFeature nCli
  nodeConfig <- parseNodeConfiguration $ unConfigPath ncFp
  (nodeLayer, nodeFeature) <- createNodeFeature loggingLayer nodeConfig nCli
  pure
    ( [loggingFeature, nodeFeature] :: [CardanoFeature],
      nodeLayer
    )
