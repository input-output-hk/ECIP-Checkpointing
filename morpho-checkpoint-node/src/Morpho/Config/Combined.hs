{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Morpho.Config.Combined where

import Barbies
import Barbies.Bare
import Cardano.Prelude
import qualified Data.Aeson as J
import Data.Functor.Compose
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Validation
import Data.Yaml
import Morpho.Common.Parsers
import Morpho.Config.Types

-- | The reason why no value could be determined for a configuration field
data ConfigError
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
  | -- | The configuration file contains an extra field that's not read
    ConfigExtraField
      { -- | The name of the field
        fieldName :: Text
      }
  deriving (Show, Eq)

-- | Turn a 'ConfigUnavailable' value into an error message that can be displayed
prettyConfigUnavailable :: ConfigError -> Text
prettyConfigUnavailable ConfigFieldParseError {fieldName, fieldParseError} =
  "- Failed to parse field " <> fieldName <> ": " <> fieldParseError
prettyConfigUnavailable ConfigMissing {fieldName, cliUnavailable = CliNoParser} =
  "- Field " <> fieldName <> " missing"
prettyConfigUnavailable ConfigMissing {fieldName, cliUnavailable = CliNotPassed opts} =
  "- Field " <> fieldName <> " missing\n  and CLI option " <> Text.intercalate "/" (NE.toList opts) <> " not passed"
prettyConfigUnavailable ConfigExtraField {fieldName}
  -- In an earlier version, the logging configuration was in the same namespace
  -- as the toplevel configuration. Throw better error when that's detected
  | fieldName `elem` loggingConfigFields =
    "- Logging configuration field " <> fieldName
      <> " newly needs to be nested under the \"Logging\" key:\n"
      <> "  Logging:\n"
      <> "    "
      <> fieldName
      <> ": ..."
  | otherwise = "- Unrecognized field " <> fieldName

-- iohk-monitoring's config fields for being able to provide a better error
-- message for them newly being nested under "Logging
loggingConfigFields :: [Text]
loggingConfigFields =
  [ "minSeverity",
    "rotation",
    "setupScribes",
    "defaultScribes",
    "setupBackends",
    "defaultBackends",
    "hasEKG",
    "hasGraylog",
    "hasPrometheus",
    "hasGUI",
    "traceForwardTo",
    "forwardDelay",
    "traceAcceptAt",
    "options"
  ]

-- An IO wrapper around 'determineConfiguration' which reads a file instead
-- and throws errors
getConfiguration :: NodeConfigurationFunctor (Either CliUnavailable) -> FilePath -> IO NodeConfiguration
getConfiguration cliConfig file = do
  mvalue <- decodeFileEither file
  case mvalue of
    Left parseErr -> do
      hPutStrLn stderr $
        "Error while parsing configuration file " <> file
          <> ": \n"
          <> prettyPrintParseException parseErr
      exitFailure
    Right value -> case determineConfiguration cliConfig value of
      Failure fails -> do
        hPutStrLn stderr $ "Configuration values were either not passed via the CLI or missing from the configuration file " <> file <> ":"
        forM_ fails $ hPutStrLn stderr . prettyConfigUnavailable
        exitFailure
      Success v -> return v

-- | Determines the final configuration by combining the value from the CLI,
-- the JSON object from the config file, and the defaults for each field.
-- Returns a 'Validation' containing a list of errors in case something went
-- wrong
determineConfiguration :: NodeConfigurationFunctor (Either CliUnavailable) -> J.Object -> Validation (NE.NonEmpty ConfigError) NodeConfiguration
determineConfiguration cliConfig configFileObject = validatedConfig <* checkNoExtraFields
  where
    -- The validated combined configuration value
    validatedConfig :: Validation (NE.NonEmpty ConfigError) NodeConfiguration
    validatedConfig = bstrip <$> bsequence' validatedFields

    -- Validation of each field separately
    validatedFields :: NodeConfigurationFunctor (Validation (NE.NonEmpty ConfigError))
    validatedFields =
      bzipWith4
        (combineConfigSources configFileObject)
        cliConfig
        configFieldName
        configFieldParser
        configFieldDefault

    -- Whether there's no extra fields in the config file
    checkNoExtraFields :: Validation (NE.NonEmpty ConfigError) ()
    checkNoExtraFields = case NE.nonEmpty extraFields of
      Nothing -> Success ()
      Just ne -> Failure (ConfigExtraField <$> ne)

    -- The fields in the config file that aren't part of the known fields, and therefore not parsed
    extraFields :: [Text]
    extraFields = HS.toList $ HM.keysSet configFileObject `HS.difference` knownConfigFields

-- | All known configuration file fields
knownConfigFields :: HS.HashSet Text
knownConfigFields = getConst $ btraverse_ (\(Const fieldName) -> Const (HS.singleton fieldName)) configFieldName

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
  Validation (NE.NonEmpty ConfigError) a
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
