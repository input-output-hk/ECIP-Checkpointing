{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Morpho.MockRpc where

import Cardano.BM.Data.Tracer
import Cardano.Prelude
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Data.Aeson
import Morpho.Ledger.PowTypes
import Morpho.RPC.Abstract
import System.Timeout

data MockNodeHandle = MockNodeHandle
  { currentPowBlockRef :: TVar (Maybe PowBlockRef),
    currentCheckPoint :: TVar (Maybe Checkpoint),
    checkPointLock :: TMVar ()
  }

addPoWBlock :: MockNodeHandle -> PowBlockRef -> IO ()
addPoWBlock MockNodeHandle {..} ref = atomically $ writeTVar currentPowBlockRef (Just ref)

-- | Creates a mock POW node
mockRpcUpstream :: IO (MockNodeHandle, RpcUpstream MockRpcEvent IO)
mockRpcUpstream = do
  varCheckpoint <- newTVarIO Nothing
  lock <- newEmptyTMVarIO
  varBlockRef <- newTVarIO Nothing
  let h = MockNodeHandle varBlockRef varCheckpoint lock
  return (h, RpcUpstream (mockCall h))

data MockRpcEvent
  deriving (Show, Generic, ToJSON, HasSeverityAnnotation)

mockCall :: MockNodeHandle -> Tracer IO MockRpcEvent -> RpcMethod i o -> i -> (o -> IO ()) -> IO ()
-- TODO: Don't ignore _k and _chkp
mockCall MockNodeHandle {..} _ GetLatestBlock (_k, _chkp) cont = readTVarIO currentPowBlockRef >>= cont
mockCall MockNodeHandle {..} _ PushCheckpoint chkp cont = do
  atomically $ do
    writeTVar currentCheckPoint (Just chkp)
    putTMVar checkPointLock ()
  cont True

waitCheckpoint :: Int -> MockNodeHandle -> IO (Maybe Checkpoint)
waitCheckpoint time MockNodeHandle {..} = timeout (time * 1000 * 1000) $ do
  Just chkp <- atomically $ do
    takeTMVar checkPointLock
    readTVar currentCheckPoint
  return chkp
