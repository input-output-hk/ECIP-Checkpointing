{-# LANGUAGE MultiParamTypeClasses #-}

module Morpho.Tracing.Types
  ( ExtractStateTrace (..),
    MorphoInitTrace (..),
  )
where

import Cardano.BM.Data.Severity
import Cardano.BM.Data.Tracer
import Cardano.Prelude
import Data.Aeson hiding (Error)
import Morpho.Config.Topology
import Morpho.Ledger.Block
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.NodeId

data MorphoInitTrace
  = NotFoundInTopology CoreNodeId
  | ProducerList CoreNodeId [RemoteAddress]
  | PerformingDBValidation
  | PrometheusException IOException
  deriving (Eq, Show)

instance HasSeverityAnnotation MorphoInitTrace where
  getSeverityAnnotation NotFoundInTopology {} = Error
  getSeverityAnnotation ProducerList {} = Info
  getSeverityAnnotation PerformingDBValidation {} = Info
  getSeverityAnnotation PrometheusException {} = Error

instance HasPrivacyAnnotation MorphoInitTrace

instance Transformable Text IO MorphoInitTrace where
  trTransformer = trStructuredText

instance ToObject MorphoInitTrace where
  toObject _ (NotFoundInTopology nid) =
    mkObject
      [ "kind" .= String "NotFoundInTopology",
        "nodeId" .= String (show nid)
      ]
  toObject _ (ProducerList nid prods) =
    mkObject
      [ "kind" .= String "ProducerList",
        "nodeId" .= String (show nid),
        "producers" .= String (show prods)
      ]
  toObject _ PerformingDBValidation =
    mkObject
      [ "kind" .= String "PerformingDBValidation"
      ]
  toObject _ (PrometheusException err) =
    mkObject
      [ "kind" .= String "PrometheusException",
        "error" .= String (show err)
      ]

instance HasTextFormatter MorphoInitTrace where
  formatText (NotFoundInTopology nid) _ = "Own node id " <> show nid <> " not found in topology"
  formatText (ProducerList nid prods) _ = "I am node id " <> show nid <> ". My producers are " <> show prods
  formatText PerformingDBValidation _ = "Performing DB validation"
  formatText (PrometheusException err) _ = "Prometheus exception: " <> show err

-- | Traces created while we extract or receive parts of the ledger state
data ExtractStateTrace h c
  = MorphoStateTrace (MorphoState (MorphoBlock h c))
  | ExtractTxErrorTrace ExtractTxError
  | VoteErrorTrace VoteError
  | WontPushCheckpointTrace (WontPushCheckpoint (MorphoBlock h c))
  deriving (Eq, Show)
