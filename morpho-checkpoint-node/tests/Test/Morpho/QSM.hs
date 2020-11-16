{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.QSM (qsmTests) where

import Prelude

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Kind (Type)
import Data.List ((\\), union)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics (Generic, Generic1)
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)
import Test.StateMachine
import Test.StateMachine.Sequential ()
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.StateMachine
import qualified Test.StateMachine.Types.Rank2 as Rank2
import Morpho.RPC.PoWMock
import Morpho.RPC.Types
import Morpho.Common.Bytes (Bytes)
import Morpho.Config.Types
import Morpho.Ledger.PowTypes
import Morpho.Node.Features.Node

qsmTests :: TestTree
qsmTests = testGroup "qsm-tests"
  [ testProperty "1 node  majority = 1" prop_1,
    testProperty "2 nodes majority = 1" prop_2,
    testProperty "2 nodes majority = 2" prop_3
  ]

newtype Node = Node Int
  deriving (Eq, Show, Ord, Generic)
  deriving newtype ToExpr

-- TODO extend the tests: test more properties of the chain.
data Chain
  = Chain
      { nextBlockNo :: PowBlockNo
      , unusedPowBlockRef :: Map PowBlockRef [Node]
      }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToExpr)

updateChain :: Config -> Chain -> Command r -> (Chain, Maybe PowBlockRef)
updateChain Config {..} Chain {..} (SendPowBlock toNodes block)
  = (chain', mElected)
  where
    eiUnused = M.alterF f block unusedPowBlockRef
    (blockNo, blocks, mElected) = case eiUnused of
      Deleted refs -> (extendBlockNo nextBlockNo, refs, Just block)
      Appened refs -> (nextBlockNo, refs, Nothing)
    chain'
      = Chain
        { nextBlockNo = blockNo
        , unusedPowBlockRef = blocks
        }
    f mOld =
      let allNodes = addToOld mOld
      in if length allNodes >= majority
        then Deleted Nothing
        else Appened (Just allNodes)

    addToOld Nothing = toNodes
    addToOld (Just oldNodes) = union oldNodes toNodes

data MapResult a = Appened a | Deleted a
  deriving Functor

newtype MockedChain = MockedChain {
  nextMockedBlockNo :: PowBlockNo
  } deriving (Eq, Show, Generic)
     deriving newtype ToExpr

type Block = PowBlockRef

data Error = Error
  deriving (Eq, Show, Generic, ToExpr)

initChain :: Chain
initChain = Chain {
    nextBlockNo = PowBlockNo checkpointInterval
  , unusedPowBlockRef = M.empty
  }

shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _ _ = []

data Model (r :: Type -> Type) = Model {
    nodes :: [Node]
  , chain :: Chain
  }
  deriving (Eq, Show, Generic)

data Command (r :: Type -> Type) =
    SendPowBlock [Node] Block
    deriving (Eq, Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

newtype Response (r :: Type -> Type) = Response
  { getResponse :: Either Error [(Node, ValidationData)] }
  deriving stock (Eq, Show, Generic1)
  deriving anyclass Rank2.Foldable

data ValidationData
  = ValidationData !PowBlockHash !Int
  | NoCheckpoint
  deriving (Eq, Show)

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
initModel mockMap = Model mockMap initChain

transition :: Config -> Model r -> Command r -> Response r -> Model r
transition cfg Model {..} cmd _resp =
  let (chain', _) = updateChain cfg chain cmd
  in Model nodes chain'

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition Model {..} _ = Top

postcondition :: Config -> Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition cfg m@Model {..} cmd@SendPowBlock {} resp =
    resp .== toMock cfg m cmd

semantics :: Config -> Map Node MockNodeHandle -> Command Concrete -> IO (Response Concrete)
semantics cfg handles (SendPowBlock toNodes blockRef) = do
    let getHandle node =
          fromMaybe (error $ "Couldn't find " ++ show node ++ " in " ++ show handles)
          (M.lookup node handles)
    let sendHandles = getHandle <$> toNodes
    forM_ sendHandles (\h -> addPoWBlock h blockRef)
    res <- waitAll cfg $ M.toList handles
    return $ Response $ Right res

-- | Wait and get the next nextpoint from all the nodes.
waitAll :: Config -> [(Node, MockNodeHandle)] -> IO [(Node, ValidationData)]
waitAll cfg = mapConcurrently $ \(node, mockHandle) -> do
  mCheckpoint <- waitCheckpoint 15 mockHandle
  let res = case mCheckpoint of
        Nothing -> NoCheckpoint
        Just chkp -> ValidationData (parentHash chkp) (majority cfg)
  return (node, res)

generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator Model {..} = Just $ do
  -- TODO extend the generators
  (toNodes, block) <- frequency
        [ (1,newBlockGen)
        , (if M.null (unusedPowBlockRef chain) then 0 else 100, existingBlockGen)]
  return $ SendPowBlock toNodes block
  where
    newBlockGen = do
        toNodes <- suchThat (sublistOf nodes) (not . null)
        let blockNo = nextBlockNo chain
        block <- genBlockRef blockNo
        return (toNodes, block)
    existingBlockGen = do
        (block, oldNodes) <- elements $ M.toList (unusedPowBlockRef chain)
        newNodes <- suchThat (sublistOf $ nodes \\ oldNodes) (not . null)
        return (newNodes, block)
    genBlockRef blockNo =
      PowBlockRef blockNo <$> arbitrary

toMock :: Config -> Model r -> Command r -> Response r
toMock cfg Model {..} cmd = Response $ Right $
  case snd $ updateChain cfg chain cmd of
    Just elected ->
      map (,ValidationData (powBlockHash elected) (majority cfg)) nodes
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

prop_1 :: Property
prop_1 = noShrinking $ withMaxSuccess 1 $
  forAllCommands (unusedSM config) (Just 2) $ \cmds -> monadicIO $ do
    nodeHandles <- liftIO $ setup 1 1
    let sm = mkSM config nodeHandles
    (hist, _model, res) <- runCommands sm cmds
    liftIO $ mapM_ cleanup nodeHandles
    prettyCommands sm hist (res === Ok)
  where
    config = Config {
        nodesNumber = 1
      , majority = 1
      }

prop_2 :: Property
prop_2 = noShrinking $ withMaxSuccess 1 $
  forAllCommands (unusedSM config) (Just 2) $ \cmds -> monadicIO $ do
    nodeHandles <- liftIO $ setup 2 2
    let sm = mkSM config nodeHandles
    (hist, _model, res) <- runCommands sm cmds
    liftIO $ mapM_ cleanup nodeHandles
    prettyCommands sm hist (res === Ok)
  where
    config = Config {
        nodesNumber = 2
      , majority = 1
      }

prop_3 :: Property
prop_3 = verbose $ noShrinking $ withMaxSuccess 3
  $ forAllCommands (unusedSM config) (Just 1)
  $ \cmds -> monadicIO $ do
    nodeHandles <- liftIO $ setup 2 3
    liftIO $ print cmds
    let sm = mkSM config nodeHandles
    (hist, _model, res) <- runCommands sm cmds
    liftIO $ mapM_ cleanup nodeHandles
    prettyCommands sm hist (res === Ok)
  where
    config = Config {
        nodesNumber = 2
      , majority = 2
      }

setup :: Int -> Int -> IO [NodeHandle]
setup nodesNum testId = do
  let testDir = mkTestDir testId
  removePathForcibly testDir
  createDirectoryIfMissing True testDir
  forM [0..(nodesNum - 1)] $ runDualNode testDir testId

cleanup :: NodeHandle -> IO ()
cleanup NodeHandle {..} = do
  cancel mainNode
  killServer mockNode

runDualNode :: String -> Int -> Int -> IO NodeHandle
runDualNode testDir testId nodeId = do
    let nodeDir = testDir ++ "/nodedir-" ++ show nodeId
    let configDir = "tests/configuration/QSM/prop_" ++ show testId
    createDirectory nodeDir

    let paths = MiscellaneousFilepaths {
        topFile = TopologyFile $ configDir ++ "/topology.json"
      , dBFile = DbFile $ nodeDir ++ "/db"
      , genesisFile = Nothing
      , signKeyFile = Nothing
      , socketFile = SocketFile $ nodeDir ++ "/.socket"
    }
    let nodeCli = NodeCLI {
        mscFp = paths
      , genesisHash = Nothing
      , nodeAddr = NodeAddress (NodeHostAddress Nothing) (fromIntegral $ 3000 + 2 * nodeId)
      , configFp = ConfigYamlFilePath $ configDir ++ "/config-" ++ show nodeId ++ ".yaml"
      , validateDB = False
      }
    mockNode <- runSimpleMock $ 8446 + 100 * testId + 2 * nodeId
    node <- async $ run nodeCli
    link node
    return $ NodeHandle nodeId mockNode node

data NodeHandle = NodeHandle {
    _nodeId :: Int
  , mockNode :: MockNodeHandle
  , mainNode :: Async ()
  }

data Config = Config {
    nodesNumber :: Int
  , majority :: Int
  }

mkTestDir :: Int -> String
mkTestDir testId = "db/qsm-tests-" ++ show testId
