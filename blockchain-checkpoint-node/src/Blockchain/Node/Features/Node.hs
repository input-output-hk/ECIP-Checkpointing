{-# LANGUAGE RankNTypes #-}

module Blockchain.Node.Features.Node
  ( NodeLayer(..)
  , createNodeFeature
  ) where

import Cardano.Prelude
import Cardano.Shell.Types (CardanoFeature (..))

import Blockchain.Config.Logging
import Blockchain.Config.Types
import Blockchain.Node.Run (runNode)

data NodeLayer = NodeLayer
  { nlRunNode :: forall m. MonadIO m => m ()
  }

createNodeFeature
  :: LoggingLayer
  -> NodeConfiguration
  -> NodeCLI
  -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer nc nCli = do
  let nodeLayer = NodeLayer {
        nlRunNode = liftIO $ runNode loggingLayer nc nCli
        }
  let cardanoFeature :: CardanoFeature
      cardanoFeature =
        CardanoFeature
        { featureName = "NodeFeature"
        , featureStart = void $ pure nodeLayer
        , featureShutdown = pure ()
        }
  pure (nodeLayer, cardanoFeature)