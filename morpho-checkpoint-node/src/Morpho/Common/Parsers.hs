{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

module Morpho.Common.Parsers
  ( nodeCliParser,
    cliTracingParser,
    -- TODO Last utilities
    lastAutoOption,
    lastDoubleOption,
    lastFlag,
    lastOption,
    lastTextListOption,
    parseGenesisPathLast,
    parseGenesisHashLast,
  )
where

import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Prelude hiding (option)
import Morpho.Config.Types
import Network.Socket (PortNumber)
import Options.Applicative
import qualified Options.Applicative as Opt

-- | The product parser for all the CLI arguments.
nodeCliParser :: Parser NodeCLI
nodeCliParser = do
  -- Filepaths
  topFp <- parseTopologyFile
  dbFp <- parseDbPath
  genFp <- optional parseGenesisPath
  sKeyFp <- optional parseSigningKey
  socketFp <- parseSocketDir
  genHash <- optional parseGenesisHash
  -- Node Address
  nAddress <- parseNodeAddress
  -- NodeConfiguration filepath
  nodeConfigFp <- parseConfigFile
  -- TraceOptions
  traceOptions <- cliTracingParserHiddenHelp
  validate <- parseValidateDB
  pure
    NodeCLI
      { mscFp =
          MiscellaneousFilepaths
            { topFile = TopologyFile topFp,
              dBFile = DbFile dbFp,
              genesisFile = GenesisFile <$> genFp,
              signKeyFile = SigningKeyFile <$> sKeyFp,
              socketFile = SocketFile socketFp
            },
        genesisHash = genHash,
        nodeAddr = nAddress,
        configFp = ConfigYamlFilePath nodeConfigFp,
        traceOpts = fromMaybe (panic "Cardano.Common.Parsers: Trace Options were not specified") $ getLast traceOptions,
        validateDB = validate
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

parseGenesisPathLast :: Parser (Last FilePath)
parseGenesisPathLast =
  lastStrOption
    ( long "genesis-file"
        <> metavar "FILEPATH"
        <> help "The filepath to the genesis file."
    )

parseGenesisPath :: Parser FilePath
parseGenesisPath =
  strOption
    ( long "genesis-file"
        <> metavar "FILEPATH"
        <> help "The filepath to the genesis file."
    )

parseSigningKey :: Parser FilePath
parseSigningKey =
  strOption
    ( long "signing-key"
        <> metavar "FILEPATH"
        <> help "Path to the signing key."
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

parseGenesisHashLast :: Parser (Last Text)
parseGenesisHashLast =
  lastStrOption
    ( long "genesis-hash"
        <> metavar "GENESIS-HASH"
        <> help "The genesis hash value."
    )

parseGenesisHash :: Parser Text
parseGenesisHash =
  strOption
    ( long "genesis-hash"
        <> metavar "GENESIS-HASH"
        <> help "The genesis hash value."
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

-- | Help hidden in order to reduce help output of `cardano-node`.
cliTracingParserHiddenHelp :: Parser (Last TraceOptions)
cliTracingParserHiddenHelp = Last . Just <$> parseTraceOptions Opt.internal

parseValidateDB :: Parser Bool
parseValidateDB =
  switch
    ( long "validate-db"
        <> help "Validate all on-disk database files"
    )

-- | Help not hidden
cliTracingParser :: Parser (Last TraceOptions)
cliTracingParser = Last . Just <$> parseTraceOptions mempty

{-------------------------------------------------------------------------------
  optparse tracing-related stuff
-------------------------------------------------------------------------------}

-- | The parser for the logging specific arguments.
parseTraceOptions :: MParser TraceOptions
parseTraceOptions m =
  TraceOptions
    <$> parseTracingVerbosity m
    <*> parseTraceChainDB m
    -- Consensus Trace Options --
    <*> parseTraceChainSyncClient m
    <*> parseTraceChainSyncHeaderServer m
    <*> parseTraceChainSyncBlockServer m
    <*> parseTraceBlockFetchDecisions m
    <*> parseTraceBlockFetchClient m
    <*> parseTraceBlockFetchServer m
    <*> parseTraceTxInbound m
    <*> parseTraceTxOutbound m
    <*> parseTraceLocalTxSubmissionServer m
    <*> parseTraceMempool m
    <*> parseTraceForge m
    -----------------------------

    -- Protocol Tracing Options --
    <*> parseTraceChainSyncProtocol m
    <*> parseTraceBlockFetchProtocol m
    <*> parseTraceBlockFetchProtocolSerialised m
    <*> parseTraceTxSubmissionProtocol m
    <*> parseTraceLocalChainSyncProtocol m
    <*> parseTraceLocalTxSubmissionProtocol m
    <*> parseTraceLocalStateQueryProtocol m
    ------------------------------

    <*> parseTraceIpSubscription m
    <*> parseTraceDnsSubscription m
    <*> parseTraceDnsResolver m
    <*> parseTraceErrorPolicy m
    <*> parseTraceMux m
    <*> parseTraceLedgerState m
    <*> parseTracePowNodeRpc m
    <*> parseTraceTimeTravelError m

parseTraceBlockFetchClient :: MParser Bool
parseTraceBlockFetchClient m =
  switch
    ( long "trace-block-fetch-client"
        <> help "Trace BlockFetch client."
        <> m
    )

parseTraceBlockFetchServer :: MParser Bool
parseTraceBlockFetchServer m =
  switch
    ( long "trace-block-fetch-server"
        <> help "Trace BlockFetch server."
        <> m
    )

parseTracingVerbosity :: MParser TracingVerbosity
parseTracingVerbosity m =
  asum
    [ flag'
        MinimalVerbosity
        ( long "tracing-verbosity-minimal"
            <> help "Minimal level of the rendering of captured items"
            <> m
        )
        <|> flag'
          MaximalVerbosity
          ( long "tracing-verbosity-maximal"
              <> help "Maximal level of the rendering of captured items"
              <> m
          )
        <|> flag
          NormalVerbosity
          NormalVerbosity
          ( long "tracing-verbosity-normal"
              <> help "the default level of the rendering of captured items"
              <> m
          )
    ]

parseTraceChainDB :: MParser Bool
parseTraceChainDB m =
  switch
    ( long "trace-chain-db"
        <> help "Verbose tracer of ChainDB."
        <> m
    )

parseTraceBlockFetchDecisions :: MParser Bool
parseTraceBlockFetchDecisions m =
  switch
    ( long "trace-block-fetch-decisions"
        <> help "Trace BlockFetch decisions made by the BlockFetch client."
        <> m
    )

parseTraceChainSyncClient :: MParser Bool
parseTraceChainSyncClient m =
  switch
    ( long "trace-chain-sync-client"
        <> help "Trace ChainSync client."
        <> m
    )

parseTraceChainSyncBlockServer :: MParser Bool
parseTraceChainSyncBlockServer m =
  switch
    ( long "trace-chain-sync-block-server"
        <> help "Trace ChainSync server (blocks)."
        <> m
    )

parseTraceChainSyncHeaderServer :: MParser Bool
parseTraceChainSyncHeaderServer m =
  switch
    ( long "trace-chain-sync-header-server"
        <> help "Trace ChainSync server (headers)."
        <> m
    )

parseTraceTxInbound :: MParser Bool
parseTraceTxInbound m =
  switch
    ( long "trace-tx-inbound"
        <> help "Trace TxSubmission server (inbound transactions)."
        <> m
    )

parseTraceTxOutbound :: MParser Bool
parseTraceTxOutbound m =
  switch
    ( long "trace-tx-outbound"
        <> help "Trace TxSubmission client (outbound transactions)."
        <> m
    )

parseTraceLocalTxSubmissionServer :: MParser Bool
parseTraceLocalTxSubmissionServer m =
  switch
    ( long "trace-local-tx-submission-server"
        <> help "Trace local TxSubmission server."
        <> m
    )

parseTraceMempool :: MParser Bool
parseTraceMempool m =
  switch
    ( long "trace-mempool"
        <> help "Trace mempool."
        <> m
    )

parseTraceForge :: MParser Bool
parseTraceForge m =
  switch
    ( long "trace-forge"
        <> help "Trace block forging."
        <> m
    )

parseTraceChainSyncProtocol :: MParser Bool
parseTraceChainSyncProtocol m =
  switch
    ( long "trace-chain-sync-protocol"
        <> help "Trace ChainSync protocol messages."
        <> m
    )

parseTraceBlockFetchProtocol :: MParser Bool
parseTraceBlockFetchProtocol m =
  switch
    ( long "trace-block-fetch-protocol"
        <> help "Trace BlockFetch protocol messages."
        <> m
    )

parseTraceBlockFetchProtocolSerialised :: MParser Bool
parseTraceBlockFetchProtocolSerialised m =
  switch
    ( long "trace-block-fetch-protocol-serialised"
        <> help "Serialised Trace BlockFetch protocol messages."
        <> m
    )

parseTraceTxSubmissionProtocol :: MParser Bool
parseTraceTxSubmissionProtocol m =
  switch
    ( long "trace-tx-submission-protocol"
        <> help "Trace TxSubmission protocol messages."
        <> m
    )

parseTraceLocalChainSyncProtocol :: MParser Bool
parseTraceLocalChainSyncProtocol m =
  switch
    ( long "trace-local-chain-sync-protocol"
        <> help "Trace local ChainSync protocol messages."
        <> m
    )

parseTraceLocalTxSubmissionProtocol :: MParser Bool
parseTraceLocalTxSubmissionProtocol m =
  switch
    ( long "trace-local-tx-submission-protocol"
        <> help "Trace local TxSubmission protocol messages."
        <> m
    )

parseTraceLocalStateQueryProtocol :: MParser Bool
parseTraceLocalStateQueryProtocol m =
  switch
    ( long "trace-local-state-query-protocol"
        <> help "Trace local State Queries."
        <> m
    )

parseTraceIpSubscription :: MParser Bool
parseTraceIpSubscription m =
  switch
    ( long "trace-ip-subscription"
        <> help "Trace IP Subscription messages."
        <> m
    )

parseTraceDnsSubscription :: MParser Bool
parseTraceDnsSubscription m =
  switch
    ( long "trace-dns-subscription"
        <> help "Trace DNS Subscription messages."
        <> m
    )

parseTraceDnsResolver :: MParser Bool
parseTraceDnsResolver m =
  switch
    ( long "trace-dns-resolver"
        <> help "Trace DNS Resolver messages."
        <> m
    )

parseTraceErrorPolicy :: MParser Bool
parseTraceErrorPolicy m =
  switch
    ( long "trace-error-policy"
        <> help "Trace error policy resolution."
        <> m
    )

parseTraceMux :: MParser Bool
parseTraceMux m =
  switch
    ( long "trace-mux"
        <> help "Trace Mux Events"
        <> m
    )

parseTracePowNodeRpc :: MParser Bool
parseTracePowNodeRpc m =
  switch
    ( long "trace-morpho-rpc"
        <> help "Trace the morpho rpc events."
        <> m
    )

parseTraceTimeTravelError :: MParser Bool
parseTraceTimeTravelError m =
  switch
    ( long "trace-timetravel-error"
        <> help "Trace timetravel errors."
        <> m
    )

parseTraceLedgerState :: MParser Bool
parseTraceLedgerState m =
  switch
    ( long "trace-ledger-state"
        <> help "Trace ledger state updates."
        <> m
    )

parseHostAddr :: Parser NodeHostAddress
parseHostAddr =
  option
    (NodeHostAddress . readMaybe <$> str)
    ( long "host-addr"
        <> metavar "HOST-NAME"
        <> help "Optionally limit node to one ipv6 or ipv4 address"
        <> (value $ NodeHostAddress Nothing)
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

lastStrOption :: IsString a => Mod OptionFields a -> Parser (Last a)
lastStrOption args = Last <$> optional (strOption args)

lastFlag :: a -> a -> Mod FlagFields a -> Parser (Last a)
lastFlag def act opts = Last <$> optional (flag def act opts)

type MParser a = (forall b c. Opt.Mod b c) -> Parser a
