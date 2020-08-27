{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}


module Blockchain.RPC.Types (
  BlockchainBlock(..),
  BlockchainBlockNumber(..),
  BlockchainBlockHash(..),
  ObftSignature(..),
  BlockchainCheckpoint(..),
  RPCResponse(..),
  RPCLatestBlockResult(..),
  RPCJSONRequest,
  RPCLatestBlockResponse,
  RPCCheckpointResponse,
  mkLatestBlockRequest,
  mkCheckpointRequest
) where

import Cardano.Prelude
import Data.Aeson
import Data.Aeson.Types
import Data.Vector as V
import Blockchain.Ledger.PowTypes

newtype BlockchainBlockNumber = BlockchainBlockNumber Text
  deriving (Generic, Eq, Show)
instance ToJSON BlockchainBlockNumber
instance FromJSON BlockchainBlockNumber

newtype BlockchainBlockHash   = BlockchainBlockHash   Text
  deriving (Generic, Eq, Show)
instance ToJSON BlockchainBlockHash
instance FromJSON BlockchainBlockHash

newtype ObftSignature       = ObftSignature       Text
  deriving (Generic, Eq, Show)
instance ToJSON ObftSignature
instance FromJSON ObftSignature


data BlockchainBlock = BlockchainBlock {
  blockNumber :: !BlockchainBlockNumber,
  blockHash   :: !BlockchainBlockHash
} deriving (Generic, Eq, Show)

instance FromJSON BlockchainBlock

data BlockchainCheckpoint = BlockchainCheckpoint {
  parentHash :: !BlockchainBlockHash,
  signatures :: [ObftSignature]
  } deriving (Generic, Eq, Show)

instance ToJSON BlockchainCheckpoint where
  toJSON (BlockchainCheckpoint (BlockchainBlockHash h) sigs) = 
    Array $ V.fromList [
      String h, 
      Array $ V.fromList $ (\(ObftSignature s) -> String s) <$> sigs
    ]

data RPCJSONRequest p = RPCJSONRequest {
  jsonrpc :: !Text,
  method  :: !Text,
  params  :: p,
  id      :: Int
} deriving (Show, Eq, Generic)

instance ToJSON (RPCJSONRequest BlockchainCheckpoint)
instance ToJSON (RPCJSONRequest [Int])

data RPCResponse r = RPCResponse {
  responseJsonrpc :: !Text,
  responseResult  :: r,
  responseid      :: Int
} deriving (Generic, Eq, Show)

data RPCLatestBlockResult = RPCLatestBlockResult {
  number :: !PowBlockNo,
  hash   :: !PowBlockHash
} deriving (Generic, Eq, Show)
instance FromJSON RPCLatestBlockResult


type RPCLatestBlockResponse = RPCResponse RPCLatestBlockResult
type RPCCheckpointResponse  = RPCResponse Bool

parseRpcResponse ::  (FromJSON r) => Value -> Parser (RPCResponse r)
parseRpcResponse = withObject "RpcResponse" $ \v ->
                RPCResponse
                    <$> v .: "jsonrpc"
                    <*> v .: "result"
                    <*> v .: "id"

instance FromJSON RPCLatestBlockResponse where
  parseJSON = parseRpcResponse
instance FromJSON RPCCheckpointResponse where
  parseJSON = parseRpcResponse

mkLatestBlockRequest :: Int -> RPCJSONRequest [Int]
mkLatestBlockRequest k = RPCJSONRequest "2.0" "checkpointing_getLatestBlock" [k] 1

mkCheckpointRequest :: BlockchainCheckpoint -> RPCJSONRequest BlockchainCheckpoint
mkCheckpointRequest chkpt = RPCJSONRequest "2.0" "checkpointing_pushCheckpoint" chkpt 1
