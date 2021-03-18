module Morpho.Tracing.Types
  ( PoWNodeRpcOperation (..),
    PoWNodeRpcTrace (..),
    ExtractStateTrace (..),
  )
where

import Cardano.Prelude
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Morpho.RPC.Types
import Prelude (String)

data PoWNodeRpcOperation = FetchLatestPoWBlock | PushCheckpoint
  deriving (Eq, Show)

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
