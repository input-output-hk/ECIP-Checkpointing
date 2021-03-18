{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Morpho.RPC.Types
  ( ObftSignature (..),
    PoWBlockchainCheckpoint (..),
    PoWNodeRPCResponse (..),
    PoWNodeJSONRequest,
    LatestPoWBlockResponse,
    PoWNodeCheckpointResponse,
    mkLatestBlockRequest,
    mkPoWNodeCheckpointRequest,
    getParams,
  )
where

import Cardano.Prelude
import Control.Monad.Fail
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Morpho.Common.Bytes as B
import Morpho.Common.Conversions
import Morpho.Ledger.PowTypes

newtype ObftSignature = ObftSignature Text
  deriving (Generic, Eq, Show)

instance ToJSON ObftSignature

instance FromJSON ObftSignature

data PoWBlockchainCheckpoint = PoWBlockchainCheckpoint
  { parentHash :: !PowBlockHash,
    signatures :: [ObftSignature]
  }
  deriving (Generic, Eq, Show)

instance ToJSON PoWBlockchainCheckpoint where
  toJSON (PoWBlockchainCheckpoint (PowBlockHash h) sigs) =
    Array $
      V.fromList
        [ String $ bytesToHex h,
          Array $ V.fromList $ (\(ObftSignature s) -> String s) <$> sigs
        ]

instance FromJSON PoWBlockchainCheckpoint where
  parseJSON = withArray "PoWBlockchainCheckpoint" $ \vector ->
    case V.toList vector of
      [h, Array vsigs] -> do
        hash <- parseJSON h
        sigs <- mapM parseSig (V.toList vsigs)
        return $ PoWBlockchainCheckpoint hash sigs
      _ -> fail "failed to parse PoWBlockchainCheckpoint"
    where
      parseSig :: Value -> Parser ObftSignature
      parseSig = withText "ObftSignature" $ return . ObftSignature

data PoWNodeJSONRequest p = PoWNodeJSONRequest
  { jsonrpc :: !Text,
    method :: !Text,
    params :: p,
    id :: Int
  }
  deriving (Show, Eq, Generic)

getParams :: PoWNodeJSONRequest p -> p
getParams = params

instance FromJSON a => FromJSON (PoWNodeJSONRequest a)

instance ToJSON a => ToJSON (PoWNodeJSONRequest a)

data PoWNodeRPCResponse r = PoWNodeRPCResponse
  { responseJsonrpc :: !Text,
    responseResult :: r,
    responseid :: Int
  }
  deriving (Generic, Eq, Show)

type LatestPoWBlockResponse = PoWNodeRPCResponse LatestBlockResponse

type PoWNodeCheckpointResponse = PoWNodeRPCResponse Bool

instance ToJSON r => ToJSON (PoWNodeRPCResponse r) where
  toJSON resp =
    object
      [ ("jsonrpc", toJSON $ responseJsonrpc resp),
        ("result", toJSON $ responseResult resp),
        ("id", toJSON $ responseid resp)
      ]

instance FromJSON r => FromJSON (PoWNodeRPCResponse r) where
  parseJSON = withObject "PoWNodeRPCResponse" $ \v ->
    PoWNodeRPCResponse
      <$> v .: "jsonrpc"
      <*> v .: "result"
      <*> v .: "id"

mkLatestBlockRequest :: Int -> PowBlockHash -> PoWNodeJSONRequest (Int, Maybe PowBlockHash)
mkLatestBlockRequest k lastCheckpointedBlockHash =
  PoWNodeJSONRequest
    "2.0"
    "checkpointing_getLatestBlock"
    (k, hash)
    1
  where
    hash =
      if unPowBlockHash lastCheckpointedBlockHash == B.empty
        then Nothing
        else Just lastCheckpointedBlockHash

mkPoWNodeCheckpointRequest :: PoWBlockchainCheckpoint -> PoWNodeJSONRequest PoWBlockchainCheckpoint
mkPoWNodeCheckpointRequest chkpt = PoWNodeJSONRequest "2.0" "checkpointing_pushCheckpoint" chkpt 1
