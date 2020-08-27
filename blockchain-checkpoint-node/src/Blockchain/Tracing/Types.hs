module Blockchain.Tracing.Types (
  RpcTrace(..),
  MorphoStateTrace(..),
  RpcOperation(..),
  TimeTravelErrorTrace(..)
) where

import Cardano.Prelude

import Blockchain.RPC.Types
import Blockchain.Ledger.Block
import Blockchain.Ledger.SnapshotTimeTravel

data RpcOperation = BlockchainFetchLatestBlock | BlockchainPushCheckpoint
  deriving (Eq, Show)

data RpcTrace =
    RpcPushedCheckpoint RPCCheckpointResponse
  | RpcLatestBlock RPCLatestBlockResponse
  | RpcNetworkError RpcOperation Text
  | RpcResponseParseError RpcOperation Text
  deriving (Eq, Show)

data MorphoStateTrace c ext = MorphoStateTrace (LedgerState (MorphoBlock c ext))
  deriving (Eq, Show)

data TimeTravelErrorTrace c ext = TimeTravelErrorTrace (LedgerStateTimeTravelError c ext)
  deriving (Show)


