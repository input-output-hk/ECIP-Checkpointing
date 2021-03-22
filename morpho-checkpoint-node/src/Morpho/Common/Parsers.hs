{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Morpho.Common.Parsers
  ( nodeCliParser,
    runCLI,
  )
where

import Barbies
import Cardano.Prelude hiding (option)
import Data.Functor.Compose
import Morpho.Config.Types
import Network.Socket (PortNumber)
import Options.Applicative
import Options.Applicative.Help ((<$$>))
import qualified Options.Applicative.Help as OptI
import Prelude (String)

runCLI :: IO (FilePath, NodeConfigurationFunctor Maybe)
runCLI = execParser opts
  where
    opts :: ParserInfo (FilePath, NodeConfigurationFunctor Maybe)
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
parserHelpHeader :: String -> Parser a -> OptI.Doc
parserHelpHeader = flip (OptI.parserUsage (prefs mempty))

-- | Produce just the options help for a given CLI option parser,
--   without the header.
parserHelpOptions :: Parser a -> OptI.Doc
parserHelpOptions = fromMaybe mempty . OptI.unChunk . OptI.fullDesc (prefs mempty)

-- | Render the help pretty document.
renderHelpDoc :: Int -> OptI.Doc -> String
renderHelpDoc cols =
  (`OptI.displayS` "") . OptI.renderPretty 1.0 cols

nodeCliParser :: Parser (FilePath, NodeConfigurationFunctor Maybe)
nodeCliParser = (,) <$> parseConfigFile <*> bsequence parseConfiguration
  where
    parseConfiguration =
      (bpure (Compose $ pure Nothing))
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
          ncSocketFile =
            parser $
              SocketFile
                <$> strOption
                  ( long "socket-dir"
                      <> metavar "FILEPATH"
                      <> help
                        "Directory with local sockets:\
                        \  ${dir}/node-{core,relay}-${node-id}.socket"
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
    parser :: Parser a -> Compose Parser Maybe a
    parser p = Compose $ fmap Just p <|> pure Nothing

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
        <> metavar "NODE-CONFIGURATION"
        <> help "Configuration file for the morpho-checkpoint-node"
        <> completer (bashCompleter "file")
    )
