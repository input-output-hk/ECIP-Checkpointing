{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.Golden
  ( goldenTests,
  )
where

import Cardano.Crypto.Hash
import Morpho.Common.Conversions
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
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
      testCase "Topology" test_golden_parseTopology,
      testCase "secretKeyNormal" test_golden_secretKeyNormal,
      testCase "secretKeyNewline" test_golden_secretKeyNewline,
      testCase "secretKeyInvalid" test_golden_secretKeyInvalid
    ]

test_golden_parseNodeConfiguration :: Assertion
test_golden_parseNodeConfiguration = do
  cfg <- parseNodeConfiguration "tests/configuration/Golden/Config.yaml"
  assertEqual "NodeConfiguration" cfg exampleNodeConfig

test_golden_parseTopology :: Assertion
test_golden_parseTopology = do
  topology <- readTopologyFile "tests/configuration/Golden/Topology.json"
  assertEqual "Topology" topology (Right exampleTopology)

test_golden_secretKeyNewline :: Assertion
test_golden_secretKeyNewline = do
  privKey <- readPrivateKey "tests/configuration/Golden/secret-key-newline"
  let expected = bytesFromHex "0093e3cf4be871137e4e11b8d94ec397afb1d3b6db4cdfef01780033b7e3c67f06" >>= importPrivateKey
  assertEqual "secret_key_1" expected privKey

test_golden_secretKeyNormal :: Assertion
test_golden_secretKeyNormal = do
  privKey <- readPrivateKey "tests/configuration/Golden/secret-key-normal"
  let expected = bytesFromHex "103becb5b909d34b5a57fc629e305c7ce8f00ef2318939e645a3c161b1d99a03" >>= importPrivateKey
  assertEqual "secret_key_2" expected privKey

test_golden_secretKeyInvalid :: Assertion
test_golden_secretKeyInvalid = do
  privKey <- readPrivateKey "tests/configuration/Golden/secret-key-invalid"
  let expected = Left "Not a valid hex string: something invalid"
  assertEqual "secret_key_2" expected privKey
