{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Morpho.Common.Parsers
  ( nodeCliParser,
    runCLI,
    CliUnavailable (..),
  )
where

import Barbies
import Cardano.Prelude hiding (option)
import Data.Functor.Compose
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Morpho.Config.Types
import Network.Socket (PortNumber)
import Options.Applicative
import qualified Options.Applicative.Common as Opt
import Options.Applicative.Help ((<$$>))
import qualified Options.Applicative.Help as Opt
import qualified Options.Applicative.Types as Opt
import Prelude (String)

runCLI :: IO (FilePath, NodeConfigurationFunctor (Either CliUnavailable))
runCLI = execParser opts
  where
    opts :: ParserInfo (FilePath, NodeConfigurationFunctor (Either CliUnavailable))
    opts =
      info
        ( nodeCliParser
            <**> helperBrief "help" "Show this help text" nodeCliHelpMain
        )
        ( fullDesc
            <> progDesc "Start a OBFT-Checkpoint node."
        )
    helperBrief :: String -> String -> String -> Parser (a -> a)
    helperBrief l d helpText =
      abortOption (InfoMsg helpText) $
        mconcat
          [ long l,
            help d
          ]
    nodeCliHelpMain :: String
    nodeCliHelpMain =
      renderHelpDoc 80 $
        parserHelpHeader "morpho-checkpoint-node" nodeCliParser
          <$$> ""
          <$$> parserHelpOptions nodeCliParser

-- | Produce just the brief help header for a given CLI option parser,
--   without the options.
parserHelpHeader :: String -> Parser a -> Opt.Doc
parserHelpHeader = flip (Opt.parserUsage (prefs mempty))

-- | Produce just the options help for a given CLI option parser,
--   without the header.
parserHelpOptions :: Parser a -> Opt.Doc
parserHelpOptions = fromMaybe mempty . Opt.unChunk . Opt.fullDesc (prefs mempty)

-- | Render the help pretty document.
renderHelpDoc :: Int -> Opt.Doc -> String
renderHelpDoc cols =
  (`Opt.displayS` "") . Opt.renderPretty 1.0 cols

nodeCliParser :: Parser (FilePath, NodeConfigurationFunctor (Either CliUnavailable))
nodeCliParser = (,) <$> parseConfigFile <*> bsequence cliParsers

-- | Reason why the value of a configuration field isn't available from the CLI
data CliUnavailable
  = -- | There's no CLI option for the field
    CliNoParser
  | -- | There is a CLI option, but it wasn't passed.
    -- Contains the options that could've been used to pass it
    CliNotPassed (NE.NonEmpty Text)
  deriving (Show, Eq)

cliParsers :: NodeConfigurationFunctor (Compose Parser (Either CliUnavailable))
cliParsers =
  (bpure (Compose (pure (Left CliNoParser))))
    { ncTopologyFile =
        parser $
          TopologyFile
            <$> strOption
              ( long "topology"
                  <> metavar "FILEPATH"
                  <> help "The path to a file describing the topology."
              ),
      ncDatabaseDir =
        parser $
          DbFile
            <$> strOption
              ( long "database-path"
                  <> metavar "FILEPATH"
                  <> help "Directory where the state is stored."
              ),
      ncNodeHost =
        parser $
          option
            (NodeHostAddress . readMaybe <$> str)
            ( long "host-addr"
                <> metavar "HOST-NAME"
                <> help "Optionally limit node to one ipv6 or ipv4 address"
            ),
      ncNodePort =
        parser $
          option
            ((fromIntegral :: Int -> PortNumber) <$> auto)
            ( long "port"
                <> metavar "PORT"
                <> help "The port number"
            ),
      ncValidateDb =
        parser $
          switch
            ( long "validate-db"
                <> help "Validate all on-disk database files"
            )
    }
  where
    -- Convenience function for defining an optional CLI parser for a field
    parser :: Parser a -> Compose Parser (Either CliUnavailable) a
    parser p =
      Compose $
        -- Right if option has been passed
        Right <$> p
          -- But if not, return Left CliNotPassed instead of throwing an error
          -- Note that this doesn't catch parse failures of a passed option
          -- (and that's what we want)
          <|> pure (Left (CliNotPassed (parserOptions p)))

    -- Extracts the option names from a CLI parser
    parserOptions :: Parser a -> NE.NonEmpty Text
    parserOptions p =
      fromMaybe (panic "No option for parser") $
        NE.nonEmpty (concat (Opt.mapParser getOptionNames p))

    -- Extracts the option names from a single option
    getOptionNames :: Opt.ArgumentReachability -> Opt.Option a -> [Text]
    getOptionNames _ o =
      Text.pack . Opt.showOption
        <$> Opt.optionNames (Opt.optMain o)

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
        <> metavar "NODE-CONFIGURATION"
        <> help "Configuration file for the morpho-checkpoint-node"
        <> completer (bashCompleter "file")
    )
