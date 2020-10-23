module Morpho.Tracing.Types
  ( PoWNodeRpcTrace (..),
    MorphoStateTrace (..),
    PoWNodeRpcOperation (..),
  )
where

import Cardano.Prelude
import Morpho.Ledger.Block
import Morpho.Ledger.State
import Morpho.RPC.Types

data PoWNodeRpcOperation = FetchLatestPoWBlock | PushCheckpoint
  deriving (Eq, Show)

data PoWNodeRpcTrace
  = RpcPushedCheckpoint PoWNodeCheckpointResponse
  | RpcLatestPoWBlock LatestPoWBlockResponse
  | RpcNetworkError PoWNodeRpcOperation Text
  | RpcResponseParseError PoWNodeRpcOperation Text
  deriving (Eq, Show)

data MorphoStateTrace h c = MorphoStateTrace (MorphoState (MorphoBlock h c))
  deriving (Eq, Show)
