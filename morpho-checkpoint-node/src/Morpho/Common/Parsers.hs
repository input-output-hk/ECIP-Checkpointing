{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Morpho.Common.Parsers
  ( nodeCliParser,
    -- TODO Last utilities
    lastAutoOption,
    lastDoubleOption,
    lastFlag,
    lastOption,
    lastTextListOption,
  )
where

import Barbies
import Cardano.Prelude hiding (option)
import Data.Functor.Compose
import Morpho.Config.Types
import Network.Socket (PortNumber)
import Options.Applicative

nodeCliParser :: Parser NodeCLI
nodeCliParser = NodeCLI <$> (ConfigYamlFilePath <$> parseConfigFile) <*> parseNodeConfig

parseNodeConfig :: Parser NodeConfigurationPartial
parseNodeConfig =
  bsequence $
    (bpure (Compose (pure Nothing)))
      { ncTopologyFile = Compose $ Just . TopologyFile <$> parseTopologyFile,
        ncDatabaseFile = Compose $ Just . DbFile <$> parseDbPath,
        ncSocketFile = Compose $ Just . SocketFile <$> parseSocketDir,
        ncNodeAddress = Compose $ Just <$> parseNodeAddress,
        ncValidateDB = Compose $ Just <$> parseValidateDB
      }

parseTopologyFile :: Parser FilePath
parseTopologyFile =
  strOption
    ( long "topology"
        <> metavar "FILEPATH"
        <> help "The path to a file describing the topology."
    )

parseDbPath :: Parser FilePath
parseDbPath =
  strOption
    ( long "database-path"
        <> metavar "FILEPATH"
        <> help "Directory where the state is stored."
    )

parseSocketDir :: Parser FilePath
parseSocketDir =
  strOption
    ( long "socket-dir"
        <> metavar "FILEPATH"
        <> help
          "Directory with local sockets:\
          \  ${dir}/node-{core,relay}-${node-id}.socket"
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
        <> metavar "NODE-CONFIGURATION"
        <> help "Configuration file for the morpho-checkpoint-node"
        <> completer (bashCompleter "file")
    )

parseValidateDB :: Parser Bool
parseValidateDB =
  switch
    ( long "validate-db"
        <> help "Validate all on-disk database files"
    )

parseHostAddr :: Parser NodeHostAddress
parseHostAddr =
  option
    (NodeHostAddress . readMaybe <$> str)
    ( long "host-addr"
        <> metavar "HOST-NAME"
        <> help "Optionally limit node to one ipv6 or ipv4 address"
        <> value (NodeHostAddress Nothing)
    )

parsePort :: Parser PortNumber
parsePort =
  option
    ((fromIntegral :: Int -> PortNumber) <$> auto)
    ( long "port"
        <> metavar "PORT"
        <> help "The port number"
    )

{-------------------------------------------------------------------------------
  optparse-applicative auxiliary
-------------------------------------------------------------------------------}

-- TODO:  deal with cardano-shell duplication

-- | Lift the parser to an optional @Last@ type.
lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

-- | General @Last@ auto option from @Read@ instance.
lastAutoOption :: Read a => Mod OptionFields a -> Parser (Last a)
lastAutoOption args = lastOption (option auto args)

lastDoubleOption :: Mod OptionFields Double -> Parser (Last Double)
lastDoubleOption = lastAutoOption

lastTextListOption :: Mod OptionFields [Text] -> Parser (Last [Text])
lastTextListOption = lastAutoOption

lastFlag :: a -> a -> Mod FlagFields a -> Parser (Last a)
lastFlag def act opts = Last <$> optional (flag def act opts)
