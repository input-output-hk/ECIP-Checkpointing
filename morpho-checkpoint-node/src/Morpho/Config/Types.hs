{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Types
  ( ConfigYamlFilePath (..),
    DbFile (..),
    DelegationCertFile (..),
    GenesisFile (..),
    MiscellaneousFilepaths (..),
    NodeCLI (..),
    NodeConfiguration (..),
    Protocol (..),
    SigningKeyFile (..),
    SocketFile (..),
    TopologyFile (..),
    TraceOptions (..),
    Update (..),
    NodeAddress (..),
    NodeHostAddress (..),
    LastKnownBlockVersion (..),
    ViewMode (..),
    nodeAddressToSockAddr,
    parseNodeConfiguration,
  )
where

import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Cardano.Prelude
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.IP as IP
import qualified Data.Text as T
import Data.Time ()
import Data.Yaml (decodeFileThrow)
import Morpho.Config.Orphans ()
import Morpho.Crypto.ECDSASignature
import Network.Socket
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import qualified Prelude

data NodeConfiguration
  = NodeConfiguration
      { ncProtocol :: Protocol,
        ncNodeId :: NodeId,
        ncNumCoreNodes :: Word64,
        ncReqNetworkMagic :: RequiresNetworkMagic,
        ncNetworkMagic :: Word32,
        ncSystemStart :: Maybe SystemStart,
        ncSecurityParameter :: Word64,
        ncLoggingSwitch :: Bool,
        ncTraceOpts :: !TraceOptions,
        ncLogMetrics :: Bool,
        ncViewMode :: ViewMode,
        ncUpdate :: Update,
        ncTimeslotLength :: SlotLength,
        ncSnapshotsOnDisk :: Int,
        ncSnapshotInterval :: Word64,
        ncPoWBlockFetchInterval :: Maybe Int,
        ncPoWNodeRpcUrl :: Text,
        ncPrometheusPort :: Int,
        -- FIXME: separate data type: CheckpointingConfiguration
        ncCheckpointInterval :: Int,
        ncRequiredMajority :: Int,
        ncFedPubKeys :: [PublicKey],
        ncNodePrivKeyFile :: FilePath
      }
  deriving (Show, Eq)

instance FromJSON NodeConfiguration where
  parseJSON = withObject "NodeConfiguration" $ \v -> do
    nId <- v .: "NodeId"
    ptcl <- v .: "Protocol"
    numCoreNode <- v .: "NumCoreNodes"
    rNetworkMagic <- v .: "RequiresNetworkMagic"
    networkMagic <- v .: "NetworkMagic"
    systemStart <- v .:? "SystemStart"
    securityParam <- v .: "SecurityParam"
    loggingSwitch <- v .: "TurnOnLogging"
    traceOptions <- traceConfigParser v
    vMode <- v .: "ViewMode"
    logMetrics <- v .: "TurnOnLogMetrics"
    -- Update Parameters
    lkBlkVersionMajor <- v .: "LastKnownBlockVersion-Major"
    lkBlkVersionMinor <- v .: "LastKnownBlockVersion-Minor"
    lkBlkVersionAlt <- v .: "LastKnownBlockVersion-Alt"
    slotLength <- v .: "SlotDuration"
    snapshotsOnDisk <- v .: "SnapshotsOnDisk"
    snapshotInterval <- v .: "SnapshotInterval"
    blockFetchInterval <- v .:? "PoWBlockFetchInterval"
    powNodeRpcUrl <- v .: "PoWNodeRpcUrl"
    promPort <- v .: "PrometheusPort"
    -- Checkpointing parameters
    checkpointInterval <- v .: "CheckpointInterval"
    requiredMajority <- v .: "RequiredMajority"
    fedPubKeys <- v .: "FedPubKeys"
    nodePrivKeyFile <- v .: "NodePrivKeyFile"
    pure $
      NodeConfiguration
        ptcl
        (CoreId (CoreNodeId nId))
        numCoreNode
        rNetworkMagic
        networkMagic
        systemStart
        securityParam
        loggingSwitch
        traceOptions
        logMetrics
        vMode
        ( Update
            ( LastKnownBlockVersion
                lkBlkVersionMajor
                lkBlkVersionMinor
                lkBlkVersionAlt
            )
        )
        slotLength
        snapshotsOnDisk
        snapshotInterval
        blockFetchInterval
        powNodeRpcUrl
        promPort
        checkpointInterval
        requiredMajority
        fedPubKeys
        nodePrivKeyFile

instance FromJSON SystemStart where
  parseJSON v = SystemStart <$> parseJSON v

traceConfigParser :: Object -> Parser TraceOptions
traceConfigParser v =
  TraceOptions
    <$> v .:? "TracingVerbosity" .!= NormalVerbosity
    <*> v .:? "TraceChainDb" .!= True
    <*> v .:? "TraceChainSyncClient" .!= True
    <*> v .:? "TraceChainSyncHeaderServer" .!= True
    <*> v .:? "TraceChainSyncBlockServer" .!= True
    <*> v .:? "TraceBlockFetchDecisions" .!= True
    <*> v .:? "TraceBlockFetchServer" .!= True
    <*> v .:? "TraceBlockFetchClient" .!= True
    <*> v .:? "TraceTxInbound" .!= True
    <*> v .:? "TraceTxOutbound" .!= True
    <*> v .:? "TraceLocalTxSubmissionServer" .!= True
    <*> v .:? "TraceMempool" .!= True
    <*> v .:? "TraceForge" .!= True
    <*> v .:? "TraceChainSyncProtocol" .!= True
    <*> v .:? "TraceBlockFetchProtocol" .!= True
    <*> v .:? "TraceBlockFetchProtocolSerialised" .!= True
    <*> v .:? "TraceTxSubmissionProtocol" .!= True
    <*> v .:? "TraceLocalChainSyncProtocol" .!= True
    <*> v .:? "TraceLocalTxSubmissionProtocol" .!= True
    <*> v .:? "traceLocalStateQueryProtocol" .!= True
    <*> v .:? "TraceIpSubscription" .!= True
    <*> v .:? "TraceDNSSubscription" .!= True
    <*> v .:? "TraceDNSResolver" .!= True
    <*> v .:? "TraceErrorPolicy" .!= True
    <*> v .:? "TraceMux" .!= True
    <*> v .:? "TraceLedgerState" .!= True
    <*> v .:? "TracePoWNodeRpc" .!= True
    <*> v .:? "TraceTimeTravelError" .!= True

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> panic $ "Parsing of TracingVerbosity failed, "
                 <> err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = panic $ "Parsing of TracingVerbosity failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)

instance FromJSON Protocol where
  parseJSON (String str) = case str of
    "MockedBFT" -> pure MockedBFT
    ptcl ->
      panic $
        "Parsing of Protocol: "
          <> ptcl
          <> " failed. "
          <> ptcl
          <> " is not a valid protocol"
  parseJSON invalid =
    panic $
      "Parsing of Protocol failed due to type mismatch. "
        <> "Encountered: "
        <> (T.pack $ Prelude.show invalid)

instance FromJSON ViewMode where
  parseJSON (String str) = case str of
    "LiveView" -> pure LiveView
    "SimpleView" -> pure SimpleView
    view ->
      panic $
        "Parsing of ViewMode: "
          <> view
          <> " failed. "
          <> view
          <> " is not a valid view mode"
  parseJSON invalid =
    panic $
      "Parsing of ViewMode failed due to type mismatch. "
        <> "Encountered: "
        <> (T.pack $ Prelude.show invalid)

data Protocol = MockedBFT
  deriving (Eq, Show)

-- Node can be run in two modes.
data ViewMode
  = LiveView -- Live mode with TUI
  | SimpleView -- Simple mode, just output text.
  deriving (Eq, Show)

parseNodeConfiguration :: FilePath -> IO NodeConfiguration
parseNodeConfiguration = decodeFileThrow

data NodeCLI
  = NodeCLI
      { mscFp :: !MiscellaneousFilepaths,
        -- TODO Use genesis file.
        genesisHash :: !(Maybe Text),
        nodeAddr :: !NodeAddress,
        configFp :: !ConfigYamlFilePath,
        validateDB :: !Bool
      }
  deriving (Show)

data MiscellaneousFilepaths
  = MiscellaneousFilepaths
      { topFile :: !TopologyFile,
        dBFile :: !DbFile,
        -- TODO Use genesis file.
        genesisFile :: !(Maybe GenesisFile),
        signKeyFile :: !(Maybe SigningKeyFile),
        socketFile :: !SocketFile
      }
  deriving (Show)

newtype TopologyFile
  = TopologyFile
      {unTopology :: FilePath}
  deriving (Show)

newtype DbFile
  = DbFile
      {unDB :: FilePath}
  deriving (Show)

newtype GenesisFile
  = GenesisFile
      {unGenesisFile :: FilePath}
  deriving (Eq, Ord, Show, IsString)

newtype DelegationCertFile
  = DelegationCertFile
      {unDelegationCert :: FilePath}
  deriving (Show)

newtype SocketFile
  = SocketFile
      {unSocket :: FilePath}
  deriving (Show)

newtype SigningKeyFile
  = SigningKeyFile
      {unSigningKey :: FilePath}
  deriving (Eq, Ord, Show, IsString)

-- TODO: migrate to Update.SoftwareVersion
newtype Update
  = Update
      { -- | Update last known block version.
        upLastKnownBlockVersion :: LastKnownBlockVersion
      }
  deriving (Eq, Show)

-- TODO: migrate to Update.ProtocolVersion
data LastKnownBlockVersion
  = LastKnownBlockVersion
      { -- | Last known block version major.
        lkbvMajor :: !Word16,
        -- | Last known block version minor.
        lkbvMinor :: !Word16,
        -- | Last known block version alternative.
        lkbvAlt :: !Word8
      }
  deriving (Eq, Show)

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case unNodeHostAddress addr of
    Just (IP.IPv4 ipv4) -> SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing -> SockAddrInet port 0 -- Could also be any IPv6 addr

-- | Detailed tracing options. Each option enables a tracer
--   which verbosity to the log output.
data TraceOptions
  = TraceOptions
      { traceVerbosity :: !TracingVerbosity,
        -- | By default we use 'readableChainDB' tracer, if on this it will use
        -- more verbose tracer
        traceChainDB :: !Bool,
        -- Consensus Tracers --
        traceChainSyncClient :: !Bool,
        traceChainSyncHeaderServer :: !Bool,
        traceChainSyncBlockServer :: !Bool,
        traceBlockFetchDecisions :: !Bool,
        traceBlockFetchClient :: !Bool,
        traceBlockFetchServer :: !Bool,
        traceTxInbound :: !Bool,
        traceTxOutbound :: !Bool,
        traceLocalTxSubmissionServer :: !Bool,
        traceMempool :: !Bool,
        traceForge :: !Bool,
        -----------------------

        -- Protocol Tracers --
        traceChainSyncProtocol :: !Bool,
        -- There's two variants of the block fetch tracer and for now
        -- at least we'll set them both together from the same flags.
        traceBlockFetchProtocol :: !Bool,
        traceBlockFetchProtocolSerialised :: !Bool,
        traceTxSubmissionProtocol :: !Bool,
        traceLocalChainSyncProtocol :: !Bool,
        traceLocalTxSubmissionProtocol :: !Bool,
        traceLocalStateQueryProtocol :: !Bool,
        traceIpSubscription :: !Bool,
        -----------------------

        traceDnsSubscription :: !Bool,
        traceDnsResolver :: !Bool,
        traceErrorPolicy :: !Bool,
        traceMux :: !Bool,
        traceLedgerState :: !Bool,
        tracePoWNodeRpc :: !Bool,
        traceTimeTravelError :: !Bool
      }
  deriving (Eq, Show)

newtype ConfigYamlFilePath
  = ConfigYamlFilePath
      {unConfigPath :: FilePath}
  deriving (Show)

-- | IPv4 address with a port number.
data NodeAddress
  = NodeAddress
      { naHostAddress :: !NodeHostAddress,
        naPort :: !PortNumber
      }
  deriving (Eq, Ord, Show)

newtype NodeHostAddress = NodeHostAddress {unNodeHostAddress :: Maybe IP.IP}
  deriving (Eq, Ord, Show)
