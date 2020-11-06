module Morpho.RPC.Request
  ( getLatestPoWBlock,
    pushPoWNodeCheckpoint,
  )
where

import Cardano.Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Morpho.RPC.Types
import Network.HTTP.Client

getLatestPoWBlock :: Text -> Int -> IO (Either Text LatestPoWBlockResponse)
getLatestPoWBlock rpcUrl k = do
  r <- request
  m <- morphoManager
  parseResponse . BL.toStrict . responseBody <$> httpLbs r m
  where
    parseResponse :: ByteString -> Either Text LatestPoWBlockResponse
    parseResponse = first T.pack . eitherDecode' . BL.fromStrict
    request :: IO Request
    request = do
      initReq <- parseRequest $ T.unpack rpcUrl -- "http://127.0.0.1:8546"
      pure
        initReq
          { method = "POST",
            requestHeaders = [("content-type", "application/json")],
            requestBody = RequestBodyLBS $ encode $ mkLatestBlockRequest k
          }

pushPoWNodeCheckpoint :: Text -> PoWBlockchainCheckpoint -> IO (Either Text PoWNodeCheckpointResponse)
pushPoWNodeCheckpoint rpcUrl mc = do
  r <- request
  m <- morphoManager
  parseResponse . BL.toStrict . responseBody <$> httpLbs r m
  where
    parseResponse :: ByteString -> Either Text PoWNodeCheckpointResponse
    parseResponse = first T.pack . eitherDecode' . BL.fromStrict
    request :: IO Request
    request = do
      initReq <- parseRequest $ T.unpack rpcUrl
      pure
        initReq
          { method = "POST",
            requestHeaders = [("content-type", "application/json")],
            requestBody = RequestBodyLBS . encode $ mkPoWNodeCheckpointRequest mc
          }

morphoManager :: IO Manager
morphoManager = newManager defaultManagerSettings