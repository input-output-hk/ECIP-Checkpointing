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
import Morpho.Tracing.Pretty (MPretty (..))
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT (BftCrypto)
import Prettyprinter (viaShow, (<+>))

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

instance MPretty MorphoInitTrace where
  mpretty (ProducerList nid prods) = "I am node id" <+> viaShow nid <> ". My producers are" <+> viaShow prods
  mpretty PerformingDBValidation = "Performing DB validation"
  mpretty (PrometheusException err) = "Prometheus exception:" <+> viaShow err

-- | Traces created while we extract or receive parts of the ledger state
data ExtractStateTrace h c
  = MorphoStateTrace (MorphoState (MorphoBlock h c))
  | VoteErrorTrace VoteError
  | WontPushCheckpointTrace (WontPushCheckpoint (MorphoBlock h c))
  deriving (Eq, Show, Generic)

instance (BftCrypto c) => MPretty (ExtractStateTrace h c) where
  mpretty (MorphoStateTrace st) = "Current Ledger State:" <+> mpretty st
  mpretty (WontPushCheckpointTrace reason) =
    "Checkpoint won't be pushed:" <+> mpretty reason
  mpretty (VoteErrorTrace err) =
    "Error while trying to create a vote:" <+> mpretty err
