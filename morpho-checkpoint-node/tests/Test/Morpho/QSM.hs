{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.QSM (qsmTests) where

import Prelude

import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic, Generic1)
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)
import Test.Morpho.Generators ()
import Test.StateMachine.Sequential
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
  [ testProperty "single node" prop_1,
    testProperty "two nodes" prop_2
  ]

newtype Node = Node Int
  deriving (Eq, Show, Ord, Generic)
  deriving newtype ToExpr

-- TODO extend the tests: test more properties of the chain.
newtype Chain = Chain {
  nextBlockNo :: PowBlockNo
  } deriving (Eq, Show, Generic)
    deriving newtype ToExpr

newtype MockedChain = MockedChain {
  nextMockedBlockNo :: PowBlockNo
  } deriving (Eq, Show, Generic)
    deriving newtype ToExpr

type Block = PowBlockRef

data Error = Error
  deriving (Eq, Show, Generic, ToExpr)

initChain :: Chain
initChain = Chain $ PowBlockNo checkpointInterval

shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _ _ = []

data Model (r :: Type -> Type) = Model {
    nodes :: Map Node MockedChain
  , chain :: Chain
  }
  deriving (Eq, Show, Generic)

data Command (r :: Type -> Type) =
    CreatePoWBlock Node Block
    deriving (Eq, Show, Generic1, Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

newtype Response (r :: Type -> Type) = Response
  { getResponse :: Either Error [(Node, ValidationData)] }
  deriving stock (Eq, Show, Generic1)
  deriving anyclass Rank2.Foldable

data ValidationData = ValidationData !PowBlockHash !Int
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

initModel :: Map Node MockedChain -> Model r
initModel mockMap = Model mockMap initChain

transition :: Model r -> Command r -> Response r -> Model r
transition m cmd resp = case (cmd, getResponse resp) of
    (CreatePoWBlock node blk, Right _) ->
      -- TODO check if there has been adoption of the block, based on the majority required
      Model {
        nodes = extendMockChain node
      , chain = Chain $ extendBlockNo $ powBlockNo blk
      }
    (CreatePoWBlock _node _blk, Left _) -> m
    where
    extendMockChain node =
      M.alter (\(Just (MockedChain n)) -> Just (MockedChain $ extendBlockNo n)) node (nodes m)

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition Model {..} _ = Top

postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition m@Model {..} cmd@CreatePoWBlock {} resp =
    toMock m cmd .== resp

semantics :: Map Node MockNodeHandle -> Command Concrete -> IO (Response Concrete)
semantics handles (CreatePoWBlock node blockRef) = do
    let mockNode = case M.lookup node handles of
          Nothing -> error $ "Couldn't find " ++ show node ++ " in " ++ show handles
          Just mNode -> mNode
    addPoWBlock mockNode blockRef
    res <- waitAll $ M.toList handles
    return $ Response $ Right res

-- | Wait and get the next nextpoint from all the nodes.
waitAll :: [(Node, MockNodeHandle)] -> IO [(Node, ValidationData)]
waitAll = mapConcurrently $ \(node, mockHandle) -> do
  checkpoint <- waitCheckpoint mockHandle
  return (node, ValidationData (parentHash checkpoint) (Prelude.length $ signatures checkpoint))

generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator Model {..} = Just $ do
-- TODO extend the generators
  (node, mockedChain) <- elements $ M.toList nodes
  let blockNo = max (nextMockedBlockNo mockedChain) (nextBlockNo chain)
  block <- genBlockRef blockNo -- (unPowBlockNo $ nextBlockNo mockedChain)
  return $ CreatePoWBlock node block
  where
    genBlockRef blockNo =
      PowBlockRef blockNo <$> arbitrary

toMock :: Model r -> Command r -> Response r
toMock m cmd = case cmd of
    CreatePoWBlock _node block ->
      Response $ Right $
        M.toList $ fmap (\_ -> ValidationData (powBlockHash block) 1) (nodes m)

mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock m cmd = return $ toMock m cmd

mkSM :: Int -> [NodeHandle] -> StateMachine Model Command IO Response
mkSM n handles = StateMachine (initModel mockMap) transition precondition postcondition
       Nothing generator shrinker (semantics $ mockNode <$> handlesMap) mock noCleanup
    where
      nodes = Node <$> [0..(n - 1)]
      handlesMap = M.fromList $ zip nodes handles
      mockMap = M.fromList $ zip nodes (replicate n (MockedChain $ PowBlockNo checkpointInterval))

unusedSM :: Int -> StateMachine Model Command IO Response
unusedSM n = mkSM n $ error "NodeHandle not used on generation or shrinking"

prop_1 :: Property
prop_1 = noShrinking $ withMaxSuccess 1 $
  forAllCommands (unusedSM 1) (Just 2) $ \cmds -> monadicIO $ do
    nodeHandles <- liftIO $ setup 1 1
    let sm = mkSM 1 nodeHandles
    (hist, _model, res) <- runCommands sm cmds
    liftIO $ mapM_ cleanup nodeHandles
    prettyCommands sm hist (res === Ok)

prop_2 :: Property
prop_2 = noShrinking $ withMaxSuccess 1 $
  forAllCommands (unusedSM 2) (Just 2) $ \cmds -> monadicIO $ do
    nodeHandles <- liftIO $ setup 2 2
    liftIO $ print cmds
    let sm = mkSM 2 nodeHandles
    (hist, _model, res) <- runCommands sm cmds
    liftIO $ mapM_ cleanup nodeHandles
    prettyCommands sm hist (res === Ok)

setup :: Int -> Int -> IO [NodeHandle]
setup nodesNum testId = do
  let testDir = mkTestDir testId
  removePathForcibly testDir
  createDirectoryIfMissing True testDir
  forM [0..(nodesNum -1)] $ runDualNode testDir testId

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

mkTestDir :: Int -> String
mkTestDir testId = "db/qsm-tests-" ++ show testId
