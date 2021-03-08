{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.Golden
  ( goldenTests,
  )
where

import Cardano.Crypto.Hash
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Ledger.Block
import Morpho.Ledger.Serialise ()
import Test.Morpho.Examples
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.Serialisation.Golden
import Prelude

instance ToGoldenDirectory MorphoNodeToNodeVersion

instance ToGoldenDirectory MorphoNodeToClientVersion

codecConfig :: CodecConfig (MorphoBlock ShortHash ConsensusMockCrypto)
codecConfig = MorphoCodecConfig ()

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden"
    [ goldenTest_all codecConfig "tests/golden-encodings" morphoExamples,
      testCase "NodeConfig" test_golden_parseNodeConfiguration,
      testCase "Topology" test_golden_parseTopology
    ]

test_golden_parseNodeConfiguration :: Assertion
test_golden_parseNodeConfiguration = do
  cfg <- parseNodeConfiguration "tests/configuration/Golden/Config.yaml"
  assertEqual "NodeConfiguration" cfg exampleNodeConfig

test_golden_parseTopology :: Assertion
test_golden_parseTopology = do
  topology <- readTopologyFile "tests/configuration/Golden/Topology.json"
  assertEqual "Topology" topology (Right exampleTopology)
