{-# LANGUAGE MultiParamTypeClasses #-}

module Morpho.Tracing.Types
  ( PoWNodeRpcOperation (..),
    PoWNodeRpcTrace (..),
    ExtractStateTrace (..),
    MorphoInitTrace (..),
  )
where

import Cardano.BM.Data.Severity
import Cardano.BM.Data.Tracer
import Cardano.Prelude
import Data.Aeson hiding (Error)
import Morpho.Config.Topology
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Morpho.RPC.Types
import Ouroboros.Consensus.NodeId
import Prelude (String)

data PoWNodeRpcOperation = FetchLatestPoWBlock | PushCheckpoint
  deriving (Eq, Show)

data MorphoInitTrace
  = NotFoundInTopology NodeAddress CoreNodeId
  | ProducerList NodeAddress CoreNodeId [RemoteAddress]
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
  toObject _ (NotFoundInTopology addr nid) =
    mkObject
      [ "kind" .= String "NotFoundInTopology",
        "addr" .= String (show addr),
        "nodeId" .= String (show nid)
      ]
  toObject _ (ProducerList addr nid prods) =
    mkObject
      [ "kind" .= String "ProducerList",
        "addr" .= String (show addr),
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
  formatText (NotFoundInTopology addr nid) _ = "Own address " <> show addr <> " Node Id " <> show nid <> " not found in topology"
  formatText (ProducerList addr nid prods) _ = "I am Node " <> show addr <> " Id: " <> show nid <> ". My producers are " <> show prods
  formatText PerformingDBValidation _ = "Performing DB validation"
  formatText (PrometheusException err) _ = "Prometheus exception: " <> show err

data PoWNodeRpcTrace
  = RpcPushedCheckpoint PoWNodeCheckpointResponse
  | RpcLatestPoWBlock LatestPoWBlockResponse
  | RpcNoLatestPoWBlock
  | RpcNetworkError PoWNodeRpcOperation String
  | RpcResponseParseError PoWNodeRpcOperation Text
  deriving (Eq, Show)

-- | Traces created while we extract or receive parts of the ledger state
data ExtractStateTrace h c
  = MorphoStateTrace (MorphoState (MorphoBlock h c))
  | PushingCheckpoint Checkpoint
  | ExtractTxErrorTrace ExtractTxError
  | WontPushCheckpointTrace (WontPushCheckpoint (MorphoBlock h c))
  deriving (Eq, Show)
