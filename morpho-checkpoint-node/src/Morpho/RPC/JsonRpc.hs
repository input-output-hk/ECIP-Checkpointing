{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implements most of json-rpc 2.0
-- See https://www.jsonrpc.org/specification
module Morpho.RPC.JsonRpc
  ( jsonRpcUpstream,
    JsonRpcEvent (..),
    JsonRpcError (..),
  )
where

import qualified Cardano.BM.Data.Severity as L
import Cardano.BM.Data.Tracer
import Cardano.Prelude
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Morpho.RPC.Abstract
import Morpho.RPC.JsonRpcProtocol
import Network.HTTP.Client

-- | A json-rpc HTTP upstream for RPC calls
jsonRpcUpstream ::
  -- | The URL to the RPC endpoint of the Pow node
  Text ->
  ExceptT JsonRpcError IO (RpcUpstream JsonRpcEvent IO)
jsonRpcUpstream rpcUrl = do
  -- TODO: Use TLS manager for TLS support
  manager <- lift $ newManager defaultManagerSettings
  baseRequest <- case parseRequest (T.unpack rpcUrl) of
    Left _ -> throwError $ JsonRpcRequestParsingFailed rpcUrl
    Right request ->
      return
        request
          { method = "POST",
            requestHeaders = [("content-type", "application/json")]
          }

  return $
    RpcUpstream $ \tracer m i cont ->
      jsonRpcRequest manager baseRequest (jsonRpcMethod m i) tracer cont

jsonRpcRequest :: forall o. Manager -> Request -> JsonRpcMethod o -> Tracer IO JsonRpcEvent -> (o -> IO ()) -> IO ()
jsonRpcRequest manager baseRequest rpcMethod tracer cont = do
  traceWith tracer $ JsonRpcEventRequest rpcRequest
  (httpLbs httpRequest manager >>= onSuccess) `catch` onHttpError
  where
    onHttpError :: HttpException -> IO ()
    onHttpError err = traceWith tracer $ JsonRpcEventError (JsonRpcHttpError err)
    onSuccess :: Response BL.ByteString -> IO ()
    onSuccess response = do
      let body = responseBody response
      traceWith tracer $ JsonRpcEventResponse body
      case eitherDecode body of
        Left err -> traceWith tracer $ JsonRpcEventError (JsonRpcResponseParsingFailed body (T.pack err))
        Right resp@JsonRpcResponse {resp_result = mresult, resp_error = merror} ->
          case (mresult, merror) of
            -- json-rpc must have either a result or an error, but not both
            (Just result, Nothing) -> case parseEither (methodResultParser rpcMethod) result of
              Left err -> traceWith tracer $ JsonRpcEventError (JsonRpcResultParsingFailed result (T.pack err))
              Right a -> cont a
            (Nothing, Just err) -> traceWith tracer $ JsonRpcEventError (JsonRpcServerError err)
            _ -> traceWith tracer $ JsonRpcEventError $ JsonRpcInvalidRpcResponse resp
    rpcRequest =
      encode
        JsonRpcRequest
          { req_jsonrpc = "2.0",
            req_method = methodName rpcMethod,
            req_params = methodParams rpcMethod,
            -- We don't need to be concerned about setting a unique id,
            -- since we handle requests synchronously
            req_id = 1
          }
    httpRequest =
      baseRequest {requestBody = RequestBodyLBS rpcRequest}

data JsonRpcEvent
  = JsonRpcEventRequest BL.ByteString
  | JsonRpcEventResponse BL.ByteString
  | JsonRpcEventError JsonRpcError
  deriving (Show, Generic)

instance HasSeverityAnnotation JsonRpcEvent where
  getSeverityAnnotation (JsonRpcEventRequest _) = L.Debug
  getSeverityAnnotation (JsonRpcEventResponse _) = L.Debug
  getSeverityAnnotation (JsonRpcEventError _) = L.Error

instance ToJSON JsonRpcEvent where
  toJSON (JsonRpcEventRequest body) =
    object
      [ "kind" .= String "JsonRpcEventRequest",
        "body" .= String (decodeUtf8 (BL.toStrict body))
      ]
  toJSON (JsonRpcEventResponse body) =
    object
      [ "kind" .= String "JsonRpcEventResponse",
        "body" .= String (decodeUtf8 (BL.toStrict body))
      ]
  toJSON (JsonRpcEventError err) =
    object
      [ "kind" .= String "JsonRpcError",
        "error" .= String (show err)
      ]

data JsonRpcError
  = JsonRpcRequestParsingFailed Text
  | JsonRpcResponseParsingFailed BL.ByteString Text
  | JsonRpcResultParsingFailed Value Text
  | JsonRpcServerError Value
  | JsonRpcInvalidRpcResponse JsonRpcResponse
  | JsonRpcHttpError HttpException
  deriving (Show, Generic)

instance ToJSON JsonRpcError where
  toJSON (JsonRpcRequestParsingFailed err) =
    object
      [ "kind" .= String "JsonRpcRequestParsingFailed",
        "error" .= String err
      ]
  toJSON (JsonRpcResponseParsingFailed body err) =
    object
      [ "kind" .= String "JsonRpcResponseParsingFailed",
        "body" .= String (decodeUtf8 (BL.toStrict body)),
        "error" .= String err
      ]
  toJSON (JsonRpcResultParsingFailed value err) =
    object
      [ "kind" .= String "JsonRpcResultParsingFailed",
        "value" .= value,
        "error" .= String err
      ]
  toJSON (JsonRpcServerError err) =
    object
      [ "kind" .= String "JsonRpcServerError",
        "error" .= err
      ]
  toJSON (JsonRpcInvalidRpcResponse resp) =
    object
      [ "kind" .= String "JsonRpcInvalidRpcResponse",
        "response" .= resp
      ]
  toJSON (JsonRpcHttpError err) =
    object
      [ "kind" .= String "JsonRpcHttpError",
        "error" .= String (T.pack (show err))
      ]

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = drop 1 . dropWhile (/= '_')
    }

data JsonRpcRequest = JsonRpcRequest
  { req_jsonrpc :: Text,
    req_method :: Text,
    req_params :: [Value],
    req_id :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON JsonRpcRequest where
  toJSON = genericToJSON jsonOptions

data JsonRpcResponse = JsonRpcResponse
  { resp_jsonrpc :: Text,
    resp_result :: Maybe Value,
    resp_error :: Maybe Value,
    resp_id :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON JsonRpcResponse where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON JsonRpcResponse where
  toJSON = genericToJSON jsonOptions
