{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.QSM
  ( qsmTests,
  )
where

import Barbies
import Control.Concurrent.Async
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class
import Data.Kind (Type)
import Data.List (union, (\\))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics (Generic, Generic1)
import Morpho.Common.Bytes (Bytes)
import Morpho.Common.Parsers
import Morpho.Config.Combined
import Morpho.Config.Types
import Morpho.Ledger.PowTypes
import Morpho.Node.Env
import Morpho.Node.Run
import System.Directory
import Test.Morpho.Generators ()
import Test.Morpho.MockRpc
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)
import Test.StateMachine
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.StateMachine.Utils
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
    [ -- testProperty "1 node  majority = 1" prop_1,
      -- testProperty "2 nodes majority = 1" prop_2,
      -- testProperty "2 nodes majority = 2" prop_3,
      testProperty "restart node" prop_4
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

shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _ _ = []

data Model (r :: Type -> Type) = Model
  { nodes :: [Node],
    chain :: Chain,
    lastCheckpoint :: Maybe PowBlockRef
  }
  deriving (Eq, Show, Generic)

data Command (r :: Type -> Type)
  = -- | Sends a Pow block to a set of PoW nodes, indicating that they either
    -- mined or received such a block
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

instance ToExpr Bytes

instance ToExpr PowBlockHash

instance ToExpr PowBlockNo

instance ToExpr PowBlockRef

instance ToExpr (Model Symbolic)

instance ToExpr (Model Concrete)

-- Should be the same as in config files
checkpointInterval :: Int
checkpointInterval = 4

extendBlockNo :: PowBlockNo -> PowBlockNo
extendBlockNo = PowBlockNo . (+ checkpointInterval) . unPowBlockNo

initModel :: [Node] -> Model r
initModel mockMap = Model mockMap initChain Nothing

transition :: Config -> Model r -> Command r -> Response r -> Model r
transition cfg Model {..} cmd _resp =
  let (chain', mElected) = updateChain cfg chain cmd
   in Model nodes chain' mElected

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition Model {} _ = Top

postcondition :: Config -> Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition cfg m@Model {} cmd@SendPowBlock {} resp =
  resp .== toMock cfg m cmd

semantics :: Config -> Map Node MockNodeHandle -> Command Concrete -> IO (Response Concrete)
semantics cfg handles (SendPowBlock toNodes blockRef) = do
  let getHandle node =
        fromMaybe
          (error $ "Couldn't find " ++ show node)
          (M.lookup node handles)
  let sendHandles = getHandle <$> toNodes
  forM_ sendHandles (`addPoWBlock` blockRef)
  res <- waitAll cfg (Just blockRef) $ M.toList handles
  return $ Response res

-- | Wait and get the next nextpoint from all the nodes.
waitAll :: Config -> Maybe PowBlockRef -> [(Node, MockNodeHandle)] -> IO [(Node, CheckpointResult)]
waitAll cfg powBlockRef = mapConcurrently $ waitNode cfg powBlockRef

waitNode :: Config -> Maybe PowBlockRef -> (Node, MockNodeHandle) -> IO (Node, CheckpointResult)
waitNode cfg powBlockRef (node, mockHandle) = do
  mCheckpoint <- waitCheckpoint 15 powBlockRef mockHandle
  let res = case mCheckpoint of
        Nothing -> NoCheckpoint
        Just chkp -> NewCheckpoint (powBlockHash $ checkpointedBlock chkp) (majority cfg)
  return (node, res)

-- TODO extend the generators
generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator Model {..} = Just $ do
  case currentCandidate chain of
    Nothing -> do
      toNodes <- suchThat (sublistOf nodes) (not . null)
      let blockNo = nextBlockNo chain
      block <- genBlockRef blockNo
      return $ SendPowBlock toNodes block
    Just (block, oldNodes) -> do
      newNodes <- suchThat (sublistOf $ nodes \\ oldNodes) (not . null)
      return $ SendPowBlock newNodes block
  where
    genBlockRef blockNo =
      PowBlockRef blockNo <$> arbitrary

toMock :: Config -> Model r -> Command r -> Response r
toMock cfg Model {..} cmd = Response $
  case snd $ updateChain cfg chain cmd of
    Just elected ->
      map (,NewCheckpoint (powBlockHash elected) (majority cfg)) nodes
    _ ->
      map (,NoCheckpoint) nodes

mock :: Config -> Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock cfg m cmd = return $ toMock cfg m cmd

mkSM :: Config -> [NodeHandle] -> StateMachine Model Command IO Response
mkSM cfg@Config {..} handles =
  StateMachine
    (initModel mockMap)
    (transition cfg)
    precondition
    (postcondition cfg)
    Nothing
    generator
    shrinker
    (semantics cfg $ mockNode <$> handlesMap)
    (mock cfg)
    noCleanup
  where
    nodes = Node <$> [0 .. (nodesNumber - 1)]
    handlesMap = M.fromList $ zip nodes handles
    mockMap = nodes

unusedSM :: Config -> StateMachine Model Command IO Response
unusedSM cfg = mkSM cfg $ error "NodeHandle not used on generation or shrinking"

-- -- | Run one node and send him block references. Test if it generates checkpoints
-- prop_1 :: Property
-- prop_1 = noShrinking $
--   withMaxSuccess 1 $
--     forAllCommands (unusedSM config) (Just 2) $
--       \cmds -> monadicIO $ do
--         nodeHandles <- liftIO $ setup 1 1
--         let sm = mkSM config nodeHandles
--         (hist, _model, res) <- runCommands sm cmds
--         liftIO $ mapM_ cleanup nodeHandles
--         prettyCommands sm hist (res === Ok)
--   where
--     config =
--       Config
--         { nodesNumber = 1,
--           majority = 1
--         }

-- -- | Same as 'prop_1', but with two nodes. We select the node to send block refs
-- -- randomly. Majority is one, so on node is enough to produce checkpoints.
-- prop_2 :: Property
-- prop_2 = noShrinking $
--   withMaxSuccess 1 $
--     forAllCommands (unusedSM config) (Just 2) $
--       \cmds -> monadicIO $ do
--         nodeHandles <- liftIO $ setup 2 2
--         let sm = mkSM config nodeHandles
--         (hist, _model, res) <- runCommands sm cmds
--         liftIO $ mapM_ cleanup nodeHandles
--         prettyCommands sm hist (res === Ok)
--   where
--     config =
--       Config
--         { nodesNumber = 2,
--           majority = 1
--         }

-- -- | Same as 'prop_2', but with majority 2. Of we send a block ref to only one, no
-- -- checkpoint should be created. If we later send to to the second we should get a checkpoint.
-- prop_3 :: Property
-- prop_3 = noShrinking $
--   withMaxSuccess 3 $
--     forAllCommands (unusedSM config) (Just 1) $
--       \cmds -> monadicIO $ do
--         nodeHandles <- liftIO $ setup 2 3
--         let sm = mkSM config nodeHandles
--         (hist, _model, res) <- runCommands sm cmds
--         liftIO $ mapM_ cleanup nodeHandles
--         prettyCommands sm hist (res === Ok)
--   where
--     config =
--       Config
--         { nodesNumber = 2,
--           majority = 2
--         }
--
--
--   withMaxSuccess 1 $
--     forAllCommands (unusedSM config) (Just 2) $
--       \cmds -> monadicIO $ do
--         nodeHandles <- liftIO $ setup 1 1
--         let sm = mkSM config nodeHandles
--         (hist, _model, res) <- runCommands sm cmds
--         liftIO $ mapM_ cleanup nodeHandles
--         prettyCommands sm hist (res === Ok)

-- | Same as 'prop_3', but creates a bigger chain and then closes both nodes and
-- reopens one of them. The target is to test if we can sync from an existing
-- chain db
prop_4 :: Property
prop_4 = noShrinking $
  withMaxSuccess 1 $
    forAllCommands (unusedSM config) (Just 15) $
      \cmds -> monadicIO $ do
        nodeHandles <- liftIO $ setup 1 4
        let sm = mkSM config nodeHandles
        (hist, model, res) <- runCommands sm cmds
        liftIO $ mapM_ cleanup nodeHandles
        h <- liftIO $ runDualNode False 4 0
        (_, chkp) <- liftIO $ waitNode config (lastCheckpoint model) (Node 0, mockNode h)
        liftIO $ cleanup h
        prettyCommands sm hist (res === Ok)
        whenFailM (return ()) $ toBlockRef chkp === (powBlockHash <$> lastCheckpoint model)
  where
    config =
      Config
        { nodesNumber = 1,
          majority = 1
        }

setup :: Int -> Int -> IO [NodeHandle]
setup nodesNum testId = do
  let testDir = mkTestDir testId
  removePathForcibly testDir
  createDirectoryIfMissing True testDir
  forM [0 .. (nodesNum - 1)] $ runDualNode True testId

cleanup :: NodeHandle -> IO ()
cleanup NodeHandle {..} = do
  cancel mainNode

runDualNode :: Bool -> Int -> Int -> IO NodeHandle
runDualNode createDir testId nodeId = do
  let testDir = mkTestDir testId
  let nodeDir = testDir ++ "/nodedir-" ++ show nodeId
  let configDir = "tests/configuration/QSM/prop_" ++ show testId
  when createDir $ createDirectory nodeDir
  let cliConfig =
        (bpure (Left CliNoParser))
          { ncTopologyFile = Right $ TopologyFile $ configDir ++ "/topology.json",
            ncDatabaseDir = Right $ DbFile $ nodeDir ++ "/db",
            ncNodePort = Right $ fromIntegral $ 3000 + 2 * nodeId,
            ncValidateDb = Right True
          }
      configFile = configDir ++ "/config-" ++ show nodeId ++ ".yaml"
  (mockNode, rpcUpstream) <- mockRpcUpstream

  nodeConfig <- getConfiguration cliConfig configFile

  node <- async $
    withEnv nodeConfig $ \(ValidatedEnv env) -> do
      run $
        ValidatedEnv $
          env
            { eRpcUpstream = rpcUpstream
            }

  link node
  return $ NodeHandle nodeId mockNode node

data NodeHandle = NodeHandle
  { _nodeId :: Int,
    mockNode :: MockNodeHandle,
    mainNode :: Async ()
  }

data Config = Config
  { nodesNumber :: Int,
    majority :: Int
  }

mkTestDir :: Int -> String
mkTestDir testId = "db/qsm-tests-" ++ show testId
