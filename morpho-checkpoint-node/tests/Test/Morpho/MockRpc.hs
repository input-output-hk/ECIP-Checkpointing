{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.MockRpc where

import Cardano.BM.Data.Tracer
import Cardano.Prelude hiding (show)
import Control.Concurrent.STM.TVar
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.TreeDiff.Class
import Data.TreeDiff.Expr
import Morpho.Common.Bytes (Bytes)
import Morpho.Ledger.PowTypes
import Morpho.RPC.Abstract
import Test.Morpho.Generators ()
import Prelude (error, show)

type ProofOfWorkHandle = TVar SomeBlockchain

addPoWBlock :: ProofOfWorkHandle -> PowBlockHash -> IO ()
addPoWBlock var hash = atomically $ modifyTVar var (mineSome hash)

-- | Creates a mock POW node
mockRpcUpstream :: IO ProofOfWorkHandle
mockRpcUpstream = newTVarIO $ SomeBlockchain GenesisBlock

createUpstream :: ProofOfWorkHandle -> RpcUpstream MockRpcEvent IO
createUpstream var = RpcUpstream (mockCall var)

newtype MockRpcEvent
  = NoCandidateFound NoCheckpointCandidateReason
  deriving (Show, Generic, ToJSON, HasSeverityAnnotation)

mockCall :: ProofOfWorkHandle -> Tracer IO MockRpcEvent -> RpcMethod i o -> i -> (o -> IO ()) -> IO ()
-- TODO: Don't ignore _k and _chkp
mockCall var tr GetLatestBlock (interval, parent) cont = do
  SomeBlockchain chain <- readTVarIO var
  case getLatestBlock chain interval parent of
    Left reason -> do
      traceWith tr $ NoCandidateFound reason
      cont Nothing
    Right result -> cont $ Just result
mockCall var _ PushCheckpoint Checkpoint {checkpointedBlock} cont = do
  result <- atomically $ do
    SomeBlockchain chain <- readTVar var
    case checkpointPow chain checkpointedBlock of
      Nothing -> return False
      Just chain' -> do
        writeTVar var chain'
        return True
  cont result

receiveChain :: ProofOfWorkHandle -> SomeBlockchain -> IO ()
receiveChain var receivedChain = atomically $ do
  existingChain <- readTVar var
  writeTVar var $ existingChain <> receivedChain

-- Returns an STM that blocks until the checkpoint of the given handle has changed
getChangeMonitor :: ProofOfWorkHandle -> IO (STM ())
getChangeMonitor var = do
  SomeBlockchain previousChain <- readTVarIO var
  let previousCheckpoint = lastCheckpointedBlock previousChain
  return $ do
    SomeBlockchain changedChain <- readTVar var
    let newCheckpoint = lastCheckpointedBlock changedChain
    when (previousCheckpoint == newCheckpoint) retry

--waitCheckpoint :: Int -> Maybe PowBlockRef -> ProofOfWorkHandle -> IO (Maybe PowBlockRef)
--waitCheckpoint time ref var = timeout (time * 1000 * 1000) $ do
--  --Just chkp <- atomically $ do
--  --  takeTMVar checkPointLock
--  --  readTVar currentCheckPoint
--  return undefined -- chkp

blockchainEq :: Blockchain a -> Blockchain b -> Bool
blockchainEq GenesisBlock GenesisBlock = True
blockchainEq (ProofOfWorkBlock resta refa) (ProofOfWorkBlock restb refb) =
  refa == refb && blockchainEq resta restb
blockchainEq (CheckpointBlock resta) (CheckpointBlock restb) = blockchainEq resta restb
blockchainEq _ _ = False

instance Eq SomeBlockchain where
  SomeBlockchain a == SomeBlockchain b = blockchainEq a b

instance Show SomeBlockchain where
  show (SomeBlockchain a) = show a

checkpointPow :: Blockchain a -> PowBlockRef -> Maybe SomeBlockchain
checkpointPow GenesisBlock _ = Nothing
checkpointPow c@(ProofOfWorkBlock rest ref) refToCheckpoint
  -- The pow block to checkpoint is after this one, so we won't find it if we recurse and can't checkpoint it
  | powBlockNo ref < powBlockNo refToCheckpoint = Nothing
  -- We found the pow block to checkpoint, do that, then return, no need to recurse further
  | ref == refToCheckpoint = Just $ SomeBlockchain $ CheckpointBlock c
  -- This is not the pow block to checkpoint, but it might be deeper down, recurse
  | otherwise = checkpointPow rest refToCheckpoint
-- If at all, the pow block to checkpoint is deeper, but if we already have a checkpoint higher up, it would just get discarded
checkpointPow (CheckpointBlock _) _ = Nothing

powBlockCounts :: [SomeBlockchain] -> Map PowBlockRef Int
powBlockCounts [] = M.empty
powBlockCounts (SomeBlockchain GenesisBlock : cs) = powBlockCounts cs
powBlockCounts (SomeBlockchain (ProofOfWorkBlock _ ref) : cs) = M.insertWith (+) ref 1 $ powBlockCounts cs
powBlockCounts (SomeBlockchain (CheckpointBlock _) : cs) = powBlockCounts cs

mineSome :: PowBlockHash -> SomeBlockchain -> SomeBlockchain
mineSome hash (SomeBlockchain b) = SomeBlockchain $ ProofOfWorkBlock b (PowBlockRef (lastBlockNumber b + 1) hash)

data SomeBlockchain = forall l.
  SomeBlockchain
  { unSomeBlockchain :: Blockchain l
  }

instance ToExpr Bytes

instance ToExpr PowBlockHash

instance ToExpr PowBlockNo

instance ToExpr PowBlockRef

instance ToExpr (Blockchain a) where
  toExpr GenesisBlock = App "GenesisBlock" []
  toExpr (ProofOfWorkBlock rest ref) = App "ProofOfWorkBlock" [toExpr rest, toExpr ref]
  toExpr (CheckpointBlock rest) = App "CheckpointBlock" [toExpr rest]

instance ToExpr SomeBlockchain where
  toExpr (SomeBlockchain x) = toExpr x

instance Semigroup SomeBlockchain where
  SomeBlockchain a <> SomeBlockchain b =
    case lastCheckpointedBlock a `compare` lastCheckpointedBlock b of
      LT -> SomeBlockchain b
      GT -> SomeBlockchain a
      EQ -> case lastBlockNumber a `compare` lastBlockNumber b of
        LT -> SomeBlockchain b
        GT -> SomeBlockchain a
        EQ -> SomeBlockchain b

lastBlockNumber :: Blockchain a -> PowBlockNo
lastBlockNumber GenesisBlock = 0
lastBlockNumber (ProofOfWorkBlock _ ref) = powBlockNo ref
lastBlockNumber (CheckpointBlock (ProofOfWorkBlock _ ref)) = 1 + powBlockNo ref

lastCheckpointedBlock :: Blockchain a -> Maybe PowBlockRef
lastCheckpointedBlock GenesisBlock = Nothing
lastCheckpointedBlock (ProofOfWorkBlock rest _) = lastCheckpointedBlock rest
lastCheckpointedBlock (CheckpointBlock (ProofOfWorkBlock _ ref)) = Just ref

--data PowBlock = PowBlock PowBlockHash | Checkpoint [Signature]
--  deriving (Eq)

data BlockType = GenesisType | ProofOfWorkType | CheckpointType

data Blockchain (latest :: BlockType) where
  GenesisBlock :: Blockchain 'GenesisType
  ProofOfWorkBlock :: Blockchain latest -> PowBlockRef -> Blockchain 'ProofOfWorkType
  CheckpointBlock :: Blockchain 'ProofOfWorkType -> Blockchain 'CheckpointType

instance Show (Blockchain latest) where
  show GenesisBlock = "Genesis"
  show (ProofOfWorkBlock rest (PowBlockRef (PowBlockNo number) (PowBlockHash bytes))) = show rest <> " | " <> show number <> "(" <> show bytes <> ")"
  show (CheckpointBlock rest) = show rest <> " | Checkpoint"

--pattern HeadC x <- x:xs where
--  HeadC x = [x]

--blockchainLength :: forall latest number. Blockchain latest number -> Natural
--blockchainLength _ = natVal (Proxy @number)

data Cmd = Mine Bytes | Chkp

commands :: [Cmd]
commands =
  [ Mine "00",
    Mine "11",
    Mine "22",
    Chkp,
    Mine "33",
    Mine "44",
    Mine "55",
    Mine "66",
    Mine "77",
    Mine "88",
    Mine "99",
    Chkp,
    Mine "aa",
    Mine "bb",
    Mine "cc"
  ]

exampleChain :: Blockchain 'ProofOfWorkType
exampleChain = go 1 GenesisBlock commands
  where
    go :: PowBlockNo -> Blockchain a -> [Cmd] -> Blockchain 'ProofOfWorkType
    go _ b@(ProofOfWorkBlock _ _) [] = b
    go _ _ [] = error "Can't do"
    go n prev (Mine bytes : cs) = go (n + 1) (ProofOfWorkBlock prev (PowBlockRef n (PowBlockHash bytes))) cs
    go n prev@(ProofOfWorkBlock _ _) (Chkp : cs) = go (n + 1) (CheckpointBlock prev) cs
    go _ _ (Chkp : _) = error "Can't do"

data NoCheckpointCandidateReason
  = NoParentButCheckpoint
  | ParentButNoCheckpoint
  | CheckpointDidntMatch
  | IntervalTooSmall
  | NotEnoughBlocks
  deriving (Eq, Show, Generic, ToJSON)

checkpointCandidates :: Maybe PowBlockRef -> Blockchain a -> Either NoCheckpointCandidateReason (Seq PowBlockRef)
checkpointCandidates Nothing GenesisBlock = Right Seq.Empty
checkpointCandidates Nothing (CheckpointBlock _) = Left NoParentButCheckpoint
checkpointCandidates (Just _) GenesisBlock = Left ParentButNoCheckpoint
checkpointCandidates (Just expRef) (CheckpointBlock (ProofOfWorkBlock _ ref))
  | expRef == ref = Right Seq.Empty
  | otherwise = Left CheckpointDidntMatch
checkpointCandidates mExpRef (ProofOfWorkBlock rest ref) =
  (Seq.:|> ref) <$> checkpointCandidates mExpRef rest

getLatestBlock :: Blockchain a -> Int -> Maybe PowBlockRef -> Either NoCheckpointCandidateReason PowBlockRef
getLatestBlock _ interval _
  | interval < 2 = Left IntervalTooSmall
getLatestBlock chain interval parent = case checkpointCandidates parent chain of
  Left reason -> Left reason
  Right candidates -> case Seq.lookup resultIndex candidates of
    Nothing -> Left NotEnoughBlocks
    Just result -> Right result
    where
      -- The index of the last element
      lastIndex = Seq.length candidates - 1
      -- The same index, but shifted to include the previously checkpointed block and the checkpoint
      lastIndex' = lastIndex + 2
      -- The latest block divisible by the interval
      resultIndex' = lastIndex' - lastIndex' `mod` interval
      -- Shift it back
      resultIndex = resultIndex' - 2

{-
  We want a checkpoint every 4'th block:
  POW <- CHK <- POW <- POW <- POW <- CHK <- POW <- ...

  But we currently have this chain:
  POW <- CHK <- POW <- POW <- POW <- POW
               [ candidates             ]    <- We have 4 candidates appearing after the previous checkpoint
                 0      1      2      3      <- Candidate list indices
                                      ^
                                  lastIndex  <- The index of the latest block

   0      1      2      3      4      5      <- candidates indices if the previously chosen
                               ^      ^         POW block and its checkpoint were included in the list (+2 blocks)
                               |      |         This makes the previously checkpointed block be at index 0,
                               |      |         making all block that could be checkpointed be at indices divisible by 4
                               |      |
                               |  lastIndex' <- We use a prime (') to indicate the shifted indexes
                               |
                               |
                          resultIndex'       <- This is the candidate for the next block to be checkpointed,
                                                as it's the latest block having an index divisible by 4

                 0      1      2      3      <- Actual candidate list indices again
                               ^
                          resultIndex        <- The resultIndex shifted back into the indices of the candidates (-2 blocks)

  Alternatively, if we only have this shorter chain:
  POW <- CHK <- POW <- POW
               [candidates]    <- We now only have 2 candidates
                 0      1
                        ^
                    lastIndex

   0      1      2      3
   ^                    ^
   |                lastIndex'
   |
resultIndex'                   <- The most recent block with index divisible by 4 is 0

  -2             0      1
   ^
resultIndex                    <- But if we shift this value back into the indices of the candidates,
                                  we will get a negative result, indicating that there is none

-}
