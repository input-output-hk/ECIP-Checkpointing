module Blockchain.Tracing.Types (
  BlockchainRpcTrace(..),
  MorphoStateTrace(..),
  BlockchainRpcOperation(..),
  TimeTravelErrorTrace(..)
) where

import Cardano.Prelude

import Blockchain.RPC.Types
import Blockchain.Ledger.Block
import Blockchain.Ledger.SnapshotTimeTravel

data BlockchainRpcOperation = BlockchainFetchLatestBlock | BlockchainPushCheckpoint
  deriving (Eq, Show)

data BlockchainRpcTrace =
    BlockchainRpcPushedCheckpoint BlockchainCheckpointResponse
  | BlockchainRpcLatestBlock BlockchainLatestBlockResponse
  | BlockchainRpcNetworkError BlockchainRpcOperation Text
  | BlockchainRpcResponseParseError BlockchainRpcOperation Text
  deriving (Eq, Show)

data MorphoStateTrace c ext = MorphoStateTrace (LedgerState (MorphoBlock c ext))
  deriving (Eq, Show)

data TimeTravelErrorTrace c ext = TimeTravelErrorTrace (LedgerStateTimeTravelError c ext)
  deriving (Show)


