{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.QSM where

import Barbies
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forM, unless)
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Foldable
import Data.Kind (Type)
import Data.List (union)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.TreeDiff.Class
import GHC.Generics (Generic, Generic1)
import Morpho.Common.Parsers
import Morpho.Config.Combined
import Morpho.Config.Types
import Morpho.Ledger.PowTypes
import Morpho.Node.Env
import Morpho.Node.Run
import System.Directory
import System.Timeout
import Test.Morpho.Generators ()
import Test.Morpho.MockRpc
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)
import Test.StateMachine hiding (StateMachine)
import Test.StateMachine.Types (StateMachine (..))
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.Tasty
import Test.Tasty.QuickCheck
import Prelude

{-
State machine tests
===================

These tests use the Morpho.RPC.PoWMock interface to simulate PoW nodes.
This module generates blocks, then sends them to the PoWMock interface, which
is then regularly queried by the morpho nodes.

-}

-- TODO extend the tests: test more properties of the chain.
qsmTests :: TestTree
qsmTests =
  testGroup
    "qsm-tests"
    [ testProperty "1 node  majority = 1" prop_1,
      testProperty "2 nodes majority = 1" prop_2,
      testProperty "2 nodes majority = 2" prop_3
      --testProperty "restart node" prop_4
    ]

newtype Node = Node Int
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToExpr)

-- | The state of the PoW chain
data Chain = Chain
  { -- | The next PoW block number that is eligible to be checkpointed
    nextBlockNo :: PowBlockNo,
    -- | Stores the PoW block that is currently in the process of being
    -- checkpointed. This means that the number of nodes is at least 1
    -- (because the checkpointing started), but less than the majority
    -- needed for a checkpoint. So in fact, since we currently only test
    -- a majority of up to 2, the list is always just a single node
    currentCandidate :: Maybe (PowBlockRef, [Node])
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToExpr)

-- Updates the chain with a command
updateChain :: Config -> Chain -> Command r -> (Chain, Maybe PowBlockRef)
updateChain Config {..} Chain {..} (SendPowBlock toNodes block) =
  (chain', mElected)
  where
    chain' =
      Chain
        { nextBlockNo = blockNo,
          currentCandidate = candidate
        }
    (blockNo, candidate, mElected) =
      if length allNodes >= majority
        then (extendBlockNo nextBlockNo, Nothing, Just block)
        else (nextBlockNo, Just (block, allNodes), Nothing)
    allNodes = case currentCandidate of
      Nothing -> toNodes
      Just (oldBlock, oldNodes)
        | oldBlock == block -> oldNodes `union` toNodes
        | otherwise ->
          error
            ( "These tests currently require all nodes to first "
                <> "receive a PoW block candidate before a new one can be received"
            )

initChain :: Chain
initChain =
  Chain
    { nextBlockNo = PowBlockNo checkpointInterval,
      currentCandidate = Nothing
    }

shrinker' :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker' _ _ = []

data Model' (r :: Type -> Type) = Model'
  { -- proof-of-work node state
    powState :: Map Node (Bool, SomeBlockchain),
    -- Morpho state, essentially
    lastCheckpointAt :: Maybe PowBlockRef
  }
  deriving (Eq, Show, Generic)

initModel'' :: Int -> Model' r
initModel'' nodeCount =
  Model'
    { powState =
        M.fromList
          [ (Node i, (False, SomeBlockchain GenesisBlock))
            | i <- [0 .. nodeCount - 1]
          ],
      lastCheckpointAt = Nothing
    }

data Command' (r :: Type -> Type)
  = StartNode Node
  | StopNode Node
  | -- Mines a block on the given node
    MineBlock Node PowBlockHash
  | -- Sends all blocks from the left node to the right chain, which can choose which part to adapt
    SendChain Node Node
  deriving (Eq, Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

data Response' (r :: Type -> Type)
  = Started
  | Stopped
  | Mined
  | NoNewCheckpoint'
  | NewCheckpoint'
  deriving stock (Eq, Show, Generic1)
  deriving anyclass (Rank2.Foldable)

toMock'' :: Model' r -> Command' r -> Response' r
toMock'' (Model' _ _) (StartNode _) = NoNewCheckpoint'
toMock'' (Model' _ _) (StopNode _) = NoNewCheckpoint'
toMock'' (Model' _ _) (MineBlock _ _) = NoNewCheckpoint'
toMock'' model cmd = snd $ update model cmd

update :: Model' r -> Command' r -> (Model' r, Response' r)
update model (StartNode node) =
  ( model
      { powState = M.adjust (first (const True)) node (powState model)
      },
    Started
  )
update model (StopNode node) =
  ( model
      { powState = M.adjust (first (const False)) node (powState model)
      },
    Stopped
  )
update model (MineBlock node hash) =
  ( model
      { powState = M.adjust (second $ \b -> mineSome hash b) node (powState model)
      },
    Mined
  )
update model (SendChain from to) = case maybeCheckpoint model' of
  Nothing -> (model', NoNewCheckpoint')
  Just model'' -> (model'', NewCheckpoint')
  where
    sendchain = snd $ powState model M.! from
    model' =
      model
        { powState = M.adjust (second $ \b -> sendchain <> b) to (powState model)
        }

-- If at least the majority of nodes have the same getLatestBlock,
-- pushes a checkpoint to all nodes if they're online and have the getLatestBlock in their chain somewhere
maybeCheckpoint :: Model' r -> Maybe (Model' r)
maybeCheckpoint model = if M.null majorities then Nothing else Just $ doCheckpoint model (fst (M.elemAt 0 majorities))
  where
    startedNodes = M.filter fst $ powState model
    powCounts = powBlockCounts $ map snd $ M.elems startedNodes
    majorityMin = M.size (powState model) `div` 2 + 1
    majorities = M.filter (>= majorityMin) powCounts

doCheckpoint :: Model' r -> PowBlockRef -> Model' r
doCheckpoint Model' {..} ref =
  Model'
    { powState = M.map updateNode powState,
      lastCheckpointAt = Just ref
    }
  where
    updateNode :: (Bool, SomeBlockchain) -> (Bool, SomeBlockchain)
    updateNode (False, c) = (False, c)
    updateNode (True, c@(SomeBlockchain chain)) = (True, fromMaybe c (checkpointPow chain ref))

data Model (r :: Type -> Type) = Model
  { nodes :: [Node],
    chain :: Chain,
    lastCheckpoint :: Maybe PowBlockRef
  }
  deriving (Eq, Show, Generic)

data Command (r :: Type -> Type)
  = --MineBlock Node

    -- | -- Sends the topmost POW block of the first node to the second node
    --  SendTopBlock Node Node
    SendPowBlock [Node] PowBlockRef
  deriving (Eq, Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

-- The response to a SendPowBlock command
newtype Response (r :: Type -> Type) = Response
  { -- | The response from each of the nodes the block was sent to
    getResponse :: [(Node, CheckpointResult)]
  }
  deriving stock (Eq, Show, Generic1)
  deriving anyclass (Rank2.Foldable)

data CheckpointResult
  = -- | A new checkpoint was created from a PoW block at a certain block number
    NewCheckpoint !PowBlockHash !Int
  | -- | No new checkpoint was created
    NoCheckpoint
  deriving (Eq, Show)

toBlockRef :: CheckpointResult -> Maybe PowBlockHash
toBlockRef (NewCheckpoint ref _) = Just ref
toBlockRef _ = Nothing

instance ToExpr (Model Symbolic)

instance ToExpr (Model Concrete)

instance ToExpr (Model' Symbolic)

instance ToExpr (Model' Concrete)

-- Should be the same as in config files
checkpointInterval :: Int
checkpointInterval = 4

extendBlockNo :: PowBlockNo -> PowBlockNo
extendBlockNo = PowBlockNo . (+ checkpointInterval) . unPowBlockNo

initModel' :: [Node] -> Model r
initModel' mockMap = Model mockMap initChain Nothing

transition' :: Config -> Model r -> Command r -> Response r -> Model r
transition' cfg Model {..} cmd _resp =
  let (chain', mElected) = updateChain cfg chain cmd
   in Model nodes chain' mElected

precondition'' :: Model' Symbolic -> Command' Symbolic -> Logic
precondition'' _ _ = Top

postcondition'' :: Model' Concrete -> Command' Concrete -> Response' Concrete -> Logic
postcondition'' m cmd resp = resp .== snd (update m cmd)

-- | Wait and get the next nextpoint from all the nodes.
-- waitAll :: Config -> [(Node, ProofOfWorkHandle)] -> IO [(Node, CheckpointResult)]
-- waitAll cfg = mapConcurrently $ waitNode cfg
--
-- waitNode :: Config -> (Node, ProofOfWorkHandle) -> IO (Node, CheckpointResult)
-- waitNode cfg (node, mockHandle) = do
--  --mCheckpoint <- waitCheckpoint 15 mockHandle
--  --let res = case mCheckpoint of
--  --      Nothing -> NoCheckpoint
--  --      Just chkp -> NewCheckpoint (powBlockHash $ checkpointedBlock chkp) (majority cfg)
--  --return (node, res)
--  undefined

{-
   -
-}

-- TODO extend the generators
generator' :: Int -> Model' Symbolic -> Maybe (Gen (Command' Symbolic))
generator' nodeCount _ =
  Just $ do
    -- If
    targetNode <- Node <$> elements [0 .. nodeCount - 1]
    frequency
      [ --(1, pure $ StartNode targetNode),
        --(1, pure $ StopNode targetNode),
        (1, MineBlock targetNode <$> arbitrary),
        ( 10,
          do
            sendNode <- Node <$> elements [0 .. nodeCount - 1]
            pure $ SendChain sendNode targetNode
        )
      ]

--case currentCandidate chain of
--  Nothing -> do
--    toNodes <- suchThat (sublistOf nodes) (not . null)
--    let blockNo = nextBlockNo chain
--    block <- genBlockRef blockNo
--    return $ SendPowBlock toNodes block
--  Just (block, oldNodes) -> do
--    newNodes <- suchThat (sublistOf $ nodes \\ oldNodes) (not . null)
--    return $ SendPowBlock newNodes block
--where
--  genBlockRef blockNo =
--    PowBlockRef blockNo <$> arbitrary

--toMock' :: Model' r -> Command' r -> Response' r
--toMock' model command = undefined

toMock :: Config -> Model r -> Command r -> Response r
toMock cfg Model {..} cmd = Response $
  case snd $ updateChain cfg chain cmd of
    Just elected ->
      map (,NewCheckpoint (powBlockHash elected) (majority cfg)) nodes
    _ ->
      map (,NoCheckpoint) nodes

mock' :: Config -> Model r -> Command r -> GenSym (Response r)
mock' cfg m cmd = return $ toMock cfg m cmd

mkSM :: Int -> Map Node NodeHandle -> StateMachine Model' Command' IO Response'
mkSM nodeCount handles =
  StateMachine
    { initModel = initModel'' nodeCount,
      transition = \model cmd _resp -> fst (update model cmd),
      precondition = precondition'',
      postcondition = postcondition'',
      invariant = Nothing,
      generator = generator' nodeCount, -- generator'',
      shrinker = shrinker'', -- shrinker'',
      semantics = semantics'' handles,
      mock = \model cmd -> return $ snd $ update model cmd,
      cleanup = cleanup'' handles
    }

shrinker'' :: Model' Symbolic -> Command' Symbolic -> [Command' Symbolic]
shrinker'' _ _ = []

cleanup'' :: Map Node NodeHandle -> Model' Concrete -> IO ()
cleanup'' handles _ = do
  traverse_ stopNode handles

-- where

--nodes = Node <$> [0 .. (nodesNumber - 1)]
--handlesMap = M.fromList $ zip nodes handles

semantics'' :: Map Node NodeHandle -> Command' Concrete -> IO (Response' Concrete)
semantics'' handles (StartNode node) = do
  startNode $ handles M.! node
  return Started
semantics'' handles (StopNode node) = do
  stopNode $ handles M.! node
  return Stopped
semantics'' handles (MineBlock node hash) = do
  let nodeHandle = handles M.! node
  addPoWBlock (powHandle nodeHandle) hash
  return NoNewCheckpoint'
semantics'' handles (SendChain from to) = do
  -- Get monitors for whether a new checkpoint is received
  newCheckpointMonitor <- asum <$> traverse (getChangeMonitor . powHandle) handles
  let fromNode = handles M.! from
      toNode = handles M.! to
  chainToSend <- readTVarIO (powHandle fromNode)
  receiveChain (powHandle toNode) chainToSend
  newCheckpoint <- isJust <$> timeout (15 * 1000 * 1000) (atomically newCheckpointMonitor)
  return $
    if newCheckpoint
      then NewCheckpoint'
      else NoNewCheckpoint'

--semantics' :: Config -> Map Node ProofOfWorkHandle -> Command Concrete -> IO (Response Concrete)
--semantics' cfg handles (SendPowBlock toNodes blockRef) = do
--  let getHandle node =
--        fromMaybe
--          (error $ "Couldn't find " ++ show node)
--          (M.lookup node handles)
--  let sendHandles = getHandle <$> toNodes
--  forM_ sendHandles (`addPoWBlock` blockRef)
--  res <- waitAll cfg $ M.toList handles
--  return $ Response res

unusedSM :: Config -> StateMachine Model' Command' IO Response'
unusedSM cfg = mkSM (nodesNumber cfg) $ error "NodeHandle not used on generation or shrinking"

-- | Run one node and send him block references. Test if it generates checkpoints
prop_1 :: Property
prop_1 = noShrinking $
  withMaxSuccess 1 $
    forAllCommands (unusedSM config) (Just 2) $
      \cmds -> monadicIO $ do
        liftIO $ print cmds
        nodeHandles <- liftIO $ setup 1 1
        let sm = mkSM (nodesNumber config) nodeHandles
        (hist, _model, res) <- runCommands sm cmds
        prettyCommands sm hist (res === Ok)
  where
    config =
      Config
        { nodesNumber = 1,
          majority = 1
        }

-- | Same as 'prop_1', but with two nodes. We select the node to send block refs
-- randomly. Majority is one, so on node is enough to produce checkpoints.
prop_2 :: Property
prop_2 = noShrinking $
  withMaxSuccess 1 $
    forAllCommands (unusedSM config) (Just 2) $
      \cmds -> monadicIO $ do
        nodeHandles <- liftIO $ setup 2 2
        let sm = mkSM (nodesNumber config) nodeHandles
        (hist, _model, res) <- runCommands sm cmds
        prettyCommands sm hist (res === Ok)
  where
    config =
      Config
        { nodesNumber = 2,
          majority = 1
        }

-- | Same as 'prop_2', but with majority 2. Of we send a block ref to only one, no
-- checkpoint should be created. If we later send to to the second we should get a checkpoint.
prop_3 :: Property
prop_3 = noShrinking $
  withMaxSuccess 3 $
    forAllCommands (unusedSM config) (Just 1) $
      \cmds -> monadicIO $ do
        nodeHandles <- liftIO $ setup 2 3
        let sm = mkSM (nodesNumber config) nodeHandles
        (hist, _model, res) <- runCommands sm cmds
        prettyCommands sm hist (res === Ok)
  where
    config =
      Config
        { nodesNumber = 2,
          majority = 2
        }

-- | Same as 'prop_3', but creates a bigger chain and then closes both nodes and
-- reopens one of them. The target is to test if we can sync from an existing
-- chain db
-- prop_4 :: Property
-- prop_4 = noShrinking $
--  withMaxSuccess 1 $
--    forAllCommands (unusedSM config) (Just 15) $
--      \cmds -> monadicIO $ do
--        nodeHandles <- liftIO $ setup 2 4
--        let sm = mkSM (nodesNumber config) nodeHandles
--        (hist, model, res) <- undefined -- runCommands sm cmds
--        liftIO $ mapM_ cleanup' nodeHandles
--        h <- liftIO $ runDualNode False 4 0
--        (_, chkp) <- liftIO $ waitNode config (Node 0, mockNode h)
--        liftIO $ cleanup' h
--        -- prettyCommands sm hist (res === Ok)
--        whenFailM (return ()) $ toBlockRef chkp === (powBlockHash <$> lastCheckpoint model)
--  where
--    config =
--      Config
--        { nodesNumber = 2,
--          majority = 2
--        }
setup :: Int -> Int -> IO (Map Node NodeHandle)
setup nodesNum testId = do
  let testDir = mkTestDir testId
  removePathForcibly testDir
  createDirectoryIfMissing True testDir
  result <- forM [0 .. nodesNum - 1] $ \nodeId -> do
    var <- newTVarIO Nothing
    handle <- mockRpcUpstream
    return
      ( Node nodeId,
        NodeHandle
          { morphoHandle = var,
            powHandle = handle,
            testId = testId,
            nodeId = nodeId
          }
      )
  return $ M.fromList result

stopNode :: NodeHandle -> IO ()
stopNode NodeHandle {..} = do
  mhandle <- readTVarIO morphoHandle
  forM_ mhandle cancel

startNode :: NodeHandle -> IO ()
startNode NodeHandle {..} = do
  a <- readTVarIO morphoHandle
  unless (isNothing a) start
  where
    start = do
      let testDir = mkTestDir testId
          nodeDir = testDir ++ "/nodedir-" ++ show nodeId
          configDir = "tests/configuration/QSM/prop_" ++ show testId
      createDirectoryIfMissing True nodeDir
      let cliConfig =
            (bpure (Left CliNoParser))
              { ncTopologyFile = Right $ TopologyFile $ configDir ++ "/topology.json",
                ncDatabaseDir = Right $ DbFile $ nodeDir ++ "/db",
                ncNodePort = Right $ fromIntegral $ 3000 + 2 * nodeId,
                ncValidateDb = Right True
              }
          configFile = configDir ++ "/config-" ++ show nodeId ++ ".yaml"

      nodeConfig <- getConfiguration cliConfig configFile

      node <- async $
        withEnv nodeConfig $ \(ValidatedEnv env) -> do
          run $
            ValidatedEnv $
              env
                { eRpcUpstream = createUpstream powHandle
                }

      link node
      atomically $ writeTVar morphoHandle $ Just node

type MorphoHandle = TVar (Maybe (Async ()))

data NodeHandle = NodeHandle
  { morphoHandle :: MorphoHandle,
    powHandle :: ProofOfWorkHandle,
    testId :: Int,
    nodeId :: Int
  }

data Config = Config
  { nodesNumber :: Int,
    majority :: Int
  }

mkTestDir :: Int -> String
mkTestDir testId = "db/qsm-tests-" ++ show testId
