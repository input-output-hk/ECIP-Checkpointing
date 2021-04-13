{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Morpho.Tracing.Types
  ( ExtractStateTrace (..),
    MorphoInitTrace (..),
  )
where

import Cardano.BM.Data.Severity
import Cardano.BM.Data.Tracer
import Cardano.Prelude
import Morpho.Config.Topology
import Morpho.Ledger.Block
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.NodeId

data MorphoInitTrace
  = ProducerList CoreNodeId [RemoteAddress]
  | PerformingDBValidation
  | PrometheusException IOException
  deriving (Eq, Show, Generic)

instance HasSeverityAnnotation MorphoInitTrace where
  getSeverityAnnotation ProducerList {} = Info
  getSeverityAnnotation PerformingDBValidation {} = Info
  getSeverityAnnotation PrometheusException {} = Error

instance HasPrivacyAnnotation MorphoInitTrace

instance HasTextFormatter MorphoInitTrace where
  formatText (ProducerList nid prods) _ = "I am node id " <> show nid <> ". My producers are " <> show prods
  formatText PerformingDBValidation _ = "Performing DB validation"
  formatText (PrometheusException err) _ = "Prometheus exception: " <> show err

-- | Traces created while we extract or receive parts of the ledger state
data ExtractStateTrace h c
  = MorphoStateTrace (MorphoState (MorphoBlock h c))
  | VoteErrorTrace VoteError
  | WontPushCheckpointTrace (WontPushCheckpoint (MorphoBlock h c))
  deriving (Eq, Show, Generic)
