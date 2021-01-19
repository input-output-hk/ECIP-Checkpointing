{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Morpho.RPC.PoWMock
  ( MockNodeHandle,
    runSimpleMock,
    addPoWBlock,
    tryReadCheckPoint,
    waitCheckpoint,
    killServer,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Aeson
import Data.Binary.Builder
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding
import Morpho.Ledger.PowTypes
import Morpho.RPC.Types
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Timeout
import Prelude

data RPCRequst
  = GetBlock (PoWNodeJSONRequest [Int])
  | PutCheckpoint (PoWNodeJSONRequest PoWBlockchainCheckpoint)

decodeRPCRequst :: BL.ByteString -> Either String RPCRequst
decodeRPCRequst bs = case decode bs of
  Just (x :: PoWNodeJSONRequest [Int]) -> Right $ GetBlock x
  Nothing -> case decode bs of
    Just (x :: PoWNodeJSONRequest PoWBlockchainCheckpoint) -> Right $ PutCheckpoint x
    Nothing -> Left $ show $ decodeUtf8 $ BL.toStrict bs

server :: MVar (Maybe PoWBlockchainCheckpoint) -> MVar () -> MVar (Maybe PowBlockRef) -> Application
server checkpoint lock block request respond = do
  bd <- getFullBody request
  case decodeRPCRequst bd of
    Right (GetBlock _req) -> do
      mBlockRef <- readMVar block
      case mBlockRef of
        Nothing ->
          respond $
            responseBuilder status404 [("content-type", "application/json")] $
              fromLazyByteString "Try again soon"
        Just blockRef -> do
          let body = PoWNodeRPCResponse "" blockRef 1
          respond $
            responseBuilder status200 [("content-type", "application/json")] $
              fromLazyByteString $
                encode body
    Right (PutCheckpoint req) -> do
      replaceMVar checkpoint (getParams req)
      -- wake up the thread waiting for the checkpoint
      putMVar lock ()
      let body = PoWNodeRPCResponse "" True 1
      respond $
        responseBuilder status200 [("content-type", "application/json")] $
          fromLazyByteString $
            encode body
    Left msg -> error $ "Unknown request " ++ msg

data MockNodeHandle = MockNodeHandle
  { port :: Int,
    threadHandle :: Async (),
    currentPowBlockRef :: MVar (Maybe PowBlockRef),
    currentCheckPoint :: MVar (Maybe PoWBlockchainCheckpoint),
    checkPointLock :: MVar ()
  }

instance Show MockNodeHandle where
  show handle = "MockNode@" ++ show (port handle)

instance Eq MockNodeHandle where
  a == b = port a == port b

runSimpleMock :: Int -> IO MockNodeHandle
runSimpleMock port = do
  varCheckpoint <- newMVar Nothing
  checkPointLock <- newEmptyMVar
  varBlockRef <- newMVar Nothing
  thread <- async $ run port (server varCheckpoint checkPointLock varBlockRef)
  link thread
  return $ MockNodeHandle port thread varBlockRef varCheckpoint checkPointLock

getFullBody :: Request -> IO BL.ByteString
getFullBody req = BL.fromStrict <$> getRequestBodyChunk req

replaceMVar :: MVar (Maybe a) -> a -> IO ()
replaceMVar var a = modifyMVar_ var $ \_ -> return (Just a)

addPoWBlock :: MockNodeHandle -> PowBlockRef -> IO ()
addPoWBlock MockNodeHandle {..} = replaceMVar currentPowBlockRef

tryReadCheckPoint :: MockNodeHandle -> IO (Maybe PoWBlockchainCheckpoint)
tryReadCheckPoint MockNodeHandle {..} = readMVar currentCheckPoint

killServer :: MockNodeHandle -> IO ()
killServer MockNodeHandle {..} = cancel threadHandle

waitCheckpoint :: Int -> MockNodeHandle -> IO (Maybe PoWBlockchainCheckpoint)
waitCheckpoint time MockNodeHandle {..} = timeout (time * 1000 * 1000) $ do
  takeMVar checkPointLock
  Just chkp <- readMVar currentCheckPoint
  return chkp
