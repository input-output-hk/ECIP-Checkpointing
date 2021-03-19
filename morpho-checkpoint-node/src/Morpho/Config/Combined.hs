{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Morpho.Config.Combined where

import Barbies
import Barbies.Bare
import Cardano.Prelude
import qualified Data.Aeson as J
import Data.Functor.Compose
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Validation
import Data.Yaml
import Morpho.Common.Parsers
import Morpho.Config.Types

-- | The reason why no value could be determined for a configuration field
data ConfigUnavailable
  = -- | Parsing the field in the configuration file failed
    ConfigFieldParseError
      { -- | The name of the field
        fieldName :: Text,
        -- | The parse error from parsing the field
        fieldParseError :: Text
      }
  | -- | The value was missing from the CLI, the config file, and doesn't have a default
    ConfigMissing
      { -- | The name of the field
        fieldName :: Text,
        -- | The reason the CLI option wasn't available
        cliUnavailable :: CliUnavailable
      }

-- | Turn a 'ConfigUnavailable' value into an error message that can be displayed
prettyConfigUnavailable :: ConfigUnavailable -> Text
prettyConfigUnavailable ConfigFieldParseError {fieldName, fieldParseError} =
  "- Failed to parse field " <> fieldName <> ": " <> fieldParseError
prettyConfigUnavailable ConfigMissing {fieldName, cliUnavailable = CliNoParser} =
  "- Field " <> fieldName <> " missing"
prettyConfigUnavailable ConfigMissing {fieldName, cliUnavailable = CliNotPassed opts} =
  "- Field " <> fieldName <> " missing\n  and CLI option " <> Text.intercalate "/" (NE.toList opts) <> " not passed"

-- An IO wrapper around 'determineConfiguration' which reads a file instead
-- and throws errors
getConfiguration :: NodeConfigurationFunctor (Either CliUnavailable) -> FilePath -> IO NodeConfiguration
getConfiguration cliConfig file = do
  value <- decodeFileThrow file
  case determineConfiguration cliConfig value of
    Failure fails -> do
      putStrLn $ "Configuration values were either not passed via the CLI or missing from the configuration file " <> file <> ":"
      forM_ fails $ putStrLn . prettyConfigUnavailable
      exitFailure
    Success v -> return v

-- | Determines the final configuration by combining the value from the CLI,
-- the JSON object from the config file, and the defaults for each field.
-- Returns a 'Validation' containing a list of errors in case something went
-- wrong
determineConfiguration :: NodeConfigurationFunctor (Either CliUnavailable) -> J.Object -> Validation (NE.NonEmpty ConfigUnavailable) NodeConfiguration
determineConfiguration cliConfig configFileObject =
  fmap bstrip $
    bsequence' $
      bzipWith4
        (combineConfigSources configFileObject)
        cliConfig
        configFieldName
        configFieldParser
        configFieldDefault

-- | Combines multiple sources of configuration together, returning either
-- an error why it wasn't available, or the final value of it
combineConfigSources ::
  -- | JSON object for the whole config file
  J.Object ->
  -- | The CLI value (or why it's not available)
  Either CliUnavailable a ->
  -- | The name of the field in the config file
  Const Text a ->
  -- | Equivalent to @J.Value -> J.Result a@, a parser function for a JSON value
  Compose ((->) J.Value) J.Result a ->
  -- | The default field value, if any
  Maybe a ->
  Validation (NE.NonEmpty ConfigUnavailable) a
combineConfigSources configFileObject cliResult (Const fieldName) (Compose parseField) maybeDefault =
  case (cliResult, parseField <$> HM.lookup fieldName configFileObject, maybeDefault) of
    -- Always throw an error if the config field can't be parsed, even if a CLI flag was passed. We don't want invalid config files
    (_, Just (J.Error err), _) -> Failure $ pure $ ConfigFieldParseError {fieldName, fieldParseError = Text.pack err}
    -- Prefer CLI over config file over defaults
    (Right cliValue, _, _) -> Success cliValue
    (_, Just (J.Success configValue), _) -> Success configValue
    (_, _, Just defaultValue) -> Success defaultValue
    -- We have neither a CLI value, nor a config value, nor a default
    (Left cliUnavailable, Nothing, Nothing) -> Failure $ pure $ ConfigMissing {fieldName, cliUnavailable}
