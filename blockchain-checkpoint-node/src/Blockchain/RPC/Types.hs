{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}


module Blockchain.RPC.Types (
  BlockchainBlock(..),
  BlockchainBlockNumber(..),
  BlockchainBlockHash(..),
  ObftSignature(..),
  BlockchainCheckpoint(..),
  BlockchainRPCResponse(..),
  BlockchainLatestBlockResult(..),
  BlockchainJSONRequest,
  BlockchainLatestBlockResponse,
  BlockchainCheckpointResponse,
  mkLatestBlockRequest,
  mkBlockchainCheckpointRequest
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

data BlockchainJSONRequest p = BlockchainJSONRequest {
  jsonrpc :: !Text,
  method  :: !Text,
  params  :: p,
  id      :: Int
} deriving (Show, Eq, Generic)

instance ToJSON (BlockchainJSONRequest BlockchainCheckpoint)
instance ToJSON (BlockchainJSONRequest [Int])

data BlockchainRPCResponse r = BlockchainRPCResponse {
  responseJsonrpc :: !Text,
  responseResult  :: r,
  responseid      :: Int
} deriving (Generic, Eq, Show)

data BlockchainLatestBlockResult = BlockchainLatestBlockResult {
  number :: !PowBlockNo,
  hash   :: !PowBlockHash
} deriving (Generic, Eq, Show)
instance FromJSON BlockchainLatestBlockResult


type BlockchainLatestBlockResponse = BlockchainRPCResponse BlockchainLatestBlockResult
type BlockchainCheckpointResponse  = BlockchainRPCResponse Bool

parseRpcResponse ::  (FromJSON r) => Value -> Parser (BlockchainRPCResponse r)
parseRpcResponse = withObject "BlockchainRpcResponse" $ \v ->
                BlockchainRPCResponse
                    <$> v .: "jsonrpc"
                    <*> v .: "result"
                    <*> v .: "id"

instance FromJSON BlockchainLatestBlockResponse where
  parseJSON = parseRpcResponse
instance FromJSON BlockchainCheckpointResponse where
  parseJSON = parseRpcResponse

mkLatestBlockRequest :: Int -> BlockchainJSONRequest [Int]
mkLatestBlockRequest k = BlockchainJSONRequest "2.0" "checkpointing_getLatestBlock" [k] 1

mkBlockchainCheckpointRequest :: BlockchainCheckpoint -> BlockchainJSONRequest BlockchainCheckpoint
mkBlockchainCheckpointRequest chkpt = BlockchainJSONRequest "2.0" "checkpointing_pushCheckpoint" chkpt 1
