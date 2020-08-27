module Blockchain.RPC.Request (
  getBlockchainLatestBlock,
  pushBlockchainCheckpoint
) where

import Cardano.Prelude
import Prelude (String)

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Network.HTTP.Client

import Blockchain.RPC.Types

getBlockchainLatestBlock :: Text -> Int -> IO (Either String RPCLatestBlockResponse)
getBlockchainLatestBlock rpcUrl k = do
  r <- request
  m <- blockchainManager
  parseResponse . BL.toStrict . responseBody <$> httpLbs r m
  where
    parseResponse :: ByteString -> Either String RPCLatestBlockResponse
    parseResponse = eitherDecode' . BL.fromStrict
    request :: IO Request
    request = do
      initReq <- parseRequest $ T.unpack rpcUrl -- "http://127.0.0.1:8546"
      pure initReq { method = "POST",
                     requestHeaders = [("content-type", "application/json")],
                     requestBody = RequestBodyLBS $ encode $ mkLatestBlockRequest k }

pushBlockchainCheckpoint :: Text -> BlockchainCheckpoint -> IO (Either String RPCCheckpointResponse)
pushBlockchainCheckpoint rpcUrl mc = do
  r <- request
  m <- blockchainManager
  parseResponse . BL.toStrict . responseBody <$> httpLbs r m
  where
    parseResponse :: ByteString -> Either String RPCCheckpointResponse
    parseResponse = eitherDecode' . BL.fromStrict
    request :: IO Request
    request = do
      initReq <- parseRequest $ T.unpack rpcUrl
      pure initReq
        { method = "POST",
          requestHeaders = [("content-type", "application/json")],
          requestBody = RequestBodyLBS . encode  $ mkCheckpointRequest mc }

blockchainManager :: IO Manager
blockchainManager = newManager defaultManagerSettings
