{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Morpho.RPC.Types
  ( PoWBlock (..),
    PoWBlockNumber (..),
    PoWBlockHash (..),
    ObftSignature (..),
    PoWBlockchainCheckpoint (..),
    PoWNodeRPCResponse (..),
    LatestPoWBlockResult (..),
    PoWNodeJSONRequest,
    LatestPoWBlockResponse,
    PoWNodeCheckpointResponse,
    mkLatestBlockRequest,
    mkPoWNodeCheckpointRequest,
  )
where

import Cardano.Prelude
import Data.Aeson
import Data.Aeson.Types
import Data.Vector as V
import Morpho.Ledger.PowTypes

newtype PoWBlockNumber = PoWBlockNumber Text
  deriving (Generic, Eq, Show)

instance ToJSON PoWBlockNumber

instance FromJSON PoWBlockNumber

newtype PoWBlockHash = PoWBlockHash Text
  deriving (Generic, Eq, Show)

instance ToJSON PoWBlockHash

instance FromJSON PoWBlockHash

newtype ObftSignature = ObftSignature Text
  deriving (Generic, Eq, Show)

instance ToJSON ObftSignature

instance FromJSON ObftSignature

data PoWBlock
  = PoWBlock
      { blockNumber :: !PoWBlockNumber,
        blockHash :: !PoWBlockHash
      }
  deriving (Generic, Eq, Show)

instance FromJSON PoWBlock

data PoWBlockchainCheckpoint
  = PoWBlockchainCheckpoint
      { parentHash :: !PoWBlockHash,
        signatures :: [ObftSignature]
      }
  deriving (Generic, Eq, Show)

instance ToJSON PoWBlockchainCheckpoint where
  toJSON (PoWBlockchainCheckpoint (PoWBlockHash h) sigs) =
    Array $
      V.fromList
        [ String h,
          Array $ V.fromList $ (\(ObftSignature s) -> String s) <$> sigs
        ]

data PoWNodeJSONRequest p
  = PoWNodeJSONRequest
      { jsonrpc :: !Text,
        method :: !Text,
        params :: p,
        id :: Int
      }
  deriving (Show, Eq, Generic)

instance ToJSON (PoWNodeJSONRequest PoWBlockchainCheckpoint)

instance ToJSON (PoWNodeJSONRequest [Int])

data PoWNodeRPCResponse r
  = PoWNodeRPCResponse
      { responseJsonrpc :: !Text,
        responseResult :: r,
        responseid :: Int
      }
  deriving (Generic, Eq, Show)

data LatestPoWBlockResult
  = LatestPoWBlockResult
      { number :: !PowBlockNo,
        hash :: !PowBlockHash
      }
  deriving (Generic, Eq, Show)

instance FromJSON LatestPoWBlockResult

type LatestPoWBlockResponse = PoWNodeRPCResponse LatestPoWBlockResult

type PoWNodeCheckpointResponse = PoWNodeRPCResponse Bool

parseRpcResponse :: (FromJSON r) => Value -> Parser (PoWNodeRPCResponse r)
parseRpcResponse = withObject "PoWNodeRPCResponse" $ \v ->
  PoWNodeRPCResponse
    <$> v .: "jsonrpc"
    <*> v .: "result"
    <*> v .: "id"

instance FromJSON LatestPoWBlockResponse where
  parseJSON = parseRpcResponse

instance FromJSON PoWNodeCheckpointResponse where
  parseJSON = parseRpcResponse

mkLatestBlockRequest :: Int -> PoWNodeJSONRequest [Int]
mkLatestBlockRequest k = PoWNodeJSONRequest "2.0" "checkpointing_getLatestBlock" [k] 1

mkPoWNodeCheckpointRequest :: PoWBlockchainCheckpoint -> PoWNodeJSONRequest PoWBlockchainCheckpoint
mkPoWNodeCheckpointRequest chkpt = PoWNodeJSONRequest "2.0" "checkpointing_pushCheckpoint" chkpt 1
