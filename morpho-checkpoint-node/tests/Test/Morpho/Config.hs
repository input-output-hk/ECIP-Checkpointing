{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Morpho.Config where

import Barbies.Bare
import Cardano.Prelude
import qualified Data.Aeson as J
import Data.Functor.Compose
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Validation
import Morpho.Common.Parsers
import Morpho.Config.Combined
import Morpho.Config.Types
import Test.Morpho.Examples
import Test.Tasty
import Test.Tasty.HUnit

configTests :: TestTree
configTests =
  testGroup
    "Config"
    [ testGroup
        "combineConfigSources"
        [ testCase "ParseFailure" combineParseFailure,
          testCase "CliOverConfig" combineCliOverConfig,
          testCase "ConfigOverDefault" combineConfigOverDefault,
          testCase "DefaultLast" combineDefaultLast,
          testCase "Missing" combineMissing
        ],
      testGroup
        "determineConfiguration"
        [ testCase "Failures" determineFailures,
          testCase "Success" determineSuccess
        ]
    ]

combineParseFailure :: Assertion
combineParseFailure = expected @?= actual
  where
    actual = combineConfigSources @Int (HM.singleton "value" (J.String "not a number")) (Right 1) (Const "value") (Compose J.fromJSON) (Just 2)
    expected = Failure (ConfigFieldParseError {fieldName = "value", fieldParseError = "parsing Int failed, expected Number, but encountered String"} NE.:| [])

combineCliOverConfig :: Assertion
combineCliOverConfig = expected @?= actual
  where
    actual = combineConfigSources @Int (HM.singleton "value" (J.Number 0)) (Right 1) (Const "value") (Compose J.fromJSON) (Just 2)
    expected = Success 1

combineConfigOverDefault :: Assertion
combineConfigOverDefault = expected @?= actual
  where
    actual = combineConfigSources @Int (HM.singleton "value" (J.Number 0)) (Left CliNoParser) (Const "value") (Compose J.fromJSON) (Just 2)
    expected = Success 0

combineDefaultLast :: Assertion
combineDefaultLast = expected @?= actual
  where
    actual = combineConfigSources @Int HM.empty (Left CliNoParser) (Const "value") (Compose J.fromJSON) (Just 2)
    expected = Success 2

combineMissing :: Assertion
combineMissing = expected @?= actual
  where
    actual = combineConfigSources @Int HM.empty (Left CliNoParser) (Const "value") (Compose J.fromJSON) Nothing
    expected = Failure (pure $ ConfigMissing {fieldName = "value", cliUnavailable = CliNoParser})

determineFailures :: Assertion
determineFailures = Failure failures @?= determineConfiguration cliConfig fileConfig
  where
    cliConfig =
      (bcoverWith Right exampleNodeConfig)
        { -- Should cause an error that the field isn't passed
          ncStableLedgerDepth = Left CliNoParser,
          -- Should cause an error that the field isn't passed, but could've been passed with CLI
          ncTopologyFile = Left (CliNotPassed ("--topology" :| [])),
          -- Shouldn't cause an error since it's passed with the config file instead
          ncTimeslotLength = Left CliNoParser,
          -- Shouldn't cause an error because it has a default
          ncPoWBlockFetchInterval = Left CliNoParser
        }
    fileConfig =
      HM.fromList
        [ ("SlotDuration", J.Number 5),
          -- Should cause a parse error since PrometheusPort should be an integer
          ("PrometheusPort", J.String "1234"),
          -- Should cause an error since this is an unrecognized field
          ("ExtraField", J.Null)
        ]
    failures =
      NE.fromList
        [ ConfigMissing {fieldName = "StableLedgerDepth", cliUnavailable = CliNoParser},
          ConfigFieldParseError {fieldName = "PrometheusPort", fieldParseError = "parsing Int failed, expected Number, but encountered String"},
          ConfigMissing {fieldName = "TopologyFile", cliUnavailable = CliNotPassed ("--topology" :| [])},
          ConfigExtraField {fieldName = "ExtraField"}
        ]

determineSuccess :: Assertion
determineSuccess = Success expected @?= determineConfiguration cliConfig fileConfig
  where
    expected = exampleNodeConfig {ncPoWBlockFetchInterval = 1000000}
    cliConfig =
      (bcoverWith Right exampleNodeConfig)
        { -- Shouldn't cause an error since it's passed with the config file instead
          ncTimeslotLength = Left CliNoParser,
          -- Shouldn't cause an error because it has a default
          ncPoWBlockFetchInterval = Left CliNoParser
        }
    fileConfig =
      HM.fromList
        [ ("SlotDuration", J.Number 2)
        ]
