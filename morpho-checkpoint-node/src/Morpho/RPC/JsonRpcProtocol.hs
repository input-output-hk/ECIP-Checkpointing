{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | json-rpc protocol definition for the interface to the PoW node
--
-- These json conversions here should be independent from the underlying data
-- structures, so as to not influence the protocol when the data structures are
-- changed
module Morpho.RPC.JsonRpcProtocol
  ( jsonRpcMethod,
    JsonRpcMethod (..),
  )
where

import Cardano.Prelude
import Control.Monad.Fail
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as Text
import qualified Morpho.Common.Bytes as B
import Morpho.Common.Conversions
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.PowTypes
import Morpho.RPC.Abstract

data JsonRpcMethod o = JsonRpcMethod
  { methodName :: Text,
    methodParams :: [Value],
    methodResultParser :: Value -> Parser o
  }

jsonRpcMethod :: RpcMethod i o -> i -> JsonRpcMethod o
jsonRpcMethod GetLatestBlock (k, PowBlockRef {powBlockHash}) =
  JsonRpcMethod
    { methodName = "checkpointing_getLatestBlock",
      methodParams = [toJSON k, toJSON (JsonRpcBlockHash <$> hash)],
      methodResultParser = fmap fromJsonRpcBlockRef . parseJSON
    }
  where
    hash =
      if unPowBlockHash powBlockHash == B.empty
        then Nothing
        else Just powBlockHash
jsonRpcMethod PushCheckpoint Checkpoint {checkpointedBlock = PowBlockRef {powBlockHash}, chkpSignatures = sigs} =
  JsonRpcMethod
    { methodName = "checkpointing_pushCheckpoint",
      methodParams =
        [ toJSON (JsonRpcBlockHash powBlockHash),
          toJSON (JsonRpcSignature <$> sigs)
        ],
      methodResultParser = withBool "Bool" return
    }

newtype JsonRpcSignature = JsonRpcSignature Signature
  deriving (Eq, Show)

instance ToJSON JsonRpcSignature where
  toJSON (JsonRpcSignature sig) = String $ sigToHex sig

newtype JsonRpcBlockNo = JsonRpcBlockNo PowBlockNo
  deriving (Eq, Show)

instance FromJSON JsonRpcBlockNo where
  parseJSON v = JsonRpcBlockNo . PowBlockNo <$> parseJSON v

newtype JsonRpcBlockHash = JsonRpcBlockHash PowBlockHash
  deriving (Eq, Show)

instance ToJSON JsonRpcBlockHash where
  toJSON (JsonRpcBlockHash (PowBlockHash bytes)) = String $ bytesToHex bytes

instance FromJSON JsonRpcBlockHash where
  parseJSON (String text) =
    case normalizeHex text of
      Just h -> case bytesFromHex h of
        Left err -> fail $ "Failed to parse block hash: " <> Text.unpack err
        Right hash -> pure $ JsonRpcBlockHash $ PowBlockHash hash
      Nothing -> fail $ "Failed to parse block hash. Invalid hash: " <> show text
  parseJSON invalid = fail $ "Failed to parse block hash due to type mismatch. Encountered: " <> show invalid

data JsonRpcBlockRef = JsonRpcBlockRef
  { number :: JsonRpcBlockNo,
    hash :: JsonRpcBlockHash
  }
  deriving (Show, Eq, Generic, FromJSON)

newtype JsonRpcLatestBlockResponse = JsonRpcLatestBlockResponse
  { block :: Maybe JsonRpcBlockRef
  }
  deriving (Show, Eq, Generic, FromJSON)

fromJsonRpcBlockRef :: JsonRpcLatestBlockResponse -> Maybe PowBlockRef
fromJsonRpcBlockRef JsonRpcLatestBlockResponse {block = Just (JsonRpcBlockRef (JsonRpcBlockNo a) (JsonRpcBlockHash b))} =
  Just $ PowBlockRef a b
fromJsonRpcBlockRef JsonRpcLatestBlockResponse {block = Nothing} = Nothing
