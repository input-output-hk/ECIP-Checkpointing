{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Types
  ( ConfigYamlFilePath (..),
    DbFile (..),
    DelegationCertFile (..),
    GenesisFile (..),
    MiscellaneousFilepaths (..),
    NodeConfiguration (..),
    Protocol (..),
    SigningKeyFile (..),
    SocketFile (..),
    TopologyFile (..),
    TraceOptions (..),
    NodeAddress (..),
    NodeHostAddress (..),
    nodeAddressToSockAddr,
    getConfiguration,
  )
where

import Barbies
import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Prelude
import Control.Monad.Fail
import Data.Aeson
import Data.Aeson.Types
import qualified Data.IP as IP
import qualified Data.Text as T
import Data.Time ()
import Data.Yaml (decodeFileThrow)
import Morpho.Config.Orphans ()
import Morpho.Crypto.ECDSASignature
import Network.Socket
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import qualified Prelude

data NodeConfiguration f = NodeConfiguration
  { ncProtocol :: f Protocol,
    ncNodeId :: f CoreNodeId,
    ncNumCoreNodes :: f Word64,
    ncNetworkMagic :: f Word32,
    ncSystemStart :: f (Maybe SystemStart),
    ncSecurityParameter :: f Word64,
    ncStableLedgerDepth :: f Int,
    ncLoggingSwitch :: f Bool,
    ncTraceOpts :: f TraceOptions,
    ncTimeslotLength :: f SlotLength,
    ncSnapshotsOnDisk :: f Int,
    ncSnapshotInterval :: f Word64,
    ncPoWBlockFetchInterval :: f Int,
    ncPoWNodeRpcUrl :: f Text,
    ncPrometheusPort :: f Int,
    -- FIXME: separate data type: CheckpointingConfiguration
    ncCheckpointInterval :: f Int,
    ncRequiredMajority :: f Int,
    ncFedPubKeys :: f [PublicKey],
    ncNodePrivKeyFile :: f FilePath,
    ncTopologyFile :: f TopologyFile,
    ncDatabaseDir :: f DbFile,
    ncSocketFile :: f SocketFile,
    ncNodeHost :: f NodeHostAddress,
    ncNodePort :: f PortNumber,
    ncValidateDb :: f Bool
  }
  deriving (Generic)

instance FunctorB NodeConfiguration

instance ApplicativeB NodeConfiguration

instance TraversableB NodeConfiguration

parseConfigFile :: Object -> NodeConfiguration Parser
parseConfigFile v =
  NodeConfiguration
    { ncProtocol = v .: "Protocol",
      ncNodeId = CoreNodeId <$> v .: "NodeId",
      ncNumCoreNodes = v .: "NumCoreNodes",
      ncNetworkMagic = v .: "NetworkMagic",
      ncSystemStart = v .:? "SystemStart",
      ncSecurityParameter = v .: "SecurityParam",
      ncStableLedgerDepth = v .: "StableLedgerDepth",
      ncLoggingSwitch = v .: "TurnOnLogging",
      ncTraceOpts = traceConfigParser v,
      ncTimeslotLength = v .: "SlotDuration",
      ncSnapshotsOnDisk = v .: "SnapshotsOnDisk",
      ncSnapshotInterval = v .: "SnapshotInterval",
      ncPoWBlockFetchInterval = v .: "PoWBlockFetchInterval",
      ncPoWNodeRpcUrl = v .: "PoWNodeRpcUrl",
      ncPrometheusPort = v .: "PrometheusPort",
      ncCheckpointInterval = v .: "CheckpointInterval",
      ncRequiredMajority = v .: "RequiredMajority",
      ncFedPubKeys = v .: "FedPubKeys",
      ncNodePrivKeyFile = v .: "NodePrivKeyFile",
      ncTopologyFile = TopologyFile <$> v .: "TopologyFile",
      ncDatabaseDir = DbFile <$> v .: "DatabaseDirectory",
      -- TODO: Remove
      ncSocketFile = SocketFile <$> v .: "SocketFile",
      ncNodeHost = NodeHostAddress . readMaybe . T.unpack <$> v .: "NodeHost",
      ncNodePort = (fromIntegral :: Int -> PortNumber) <$> v .: "NodePort",
      ncValidateDb = v .: "ValidateDatabase"
    }

defaultConfiguration :: NodeConfiguration Maybe
defaultConfiguration =
  (bpure Nothing)
    { ncSystemStart = Just Nothing,
      ncLoggingSwitch = Just True,
      ncSnapshotsOnDisk = Just 60,
      ncSnapshotInterval = Just 60,
      ncPoWBlockFetchInterval = Just 1000000,
      ncNodeHost = Just (NodeHostAddress Nothing),
      ncValidateDb = Just False
    }

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
    <*> v .:? "TraceHandshake" .!= True
    <*> v .:? "TraceLedgerState" .!= True
    <*> v .:? "TracePoWNodeRpc" .!= True
    <*> v .:? "TraceTimeTravelError" .!= True

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err ->
      panic $
        "Parsing of TracingVerbosity failed, "
          <> err
          <> " is not a valid TracingVerbosity"
  parseJSON invalid =
    panic $
      "Parsing of TracingVerbosity failed due to type mismatch. "
        <> "Encountered: "
        <> T.pack (Prelude.show invalid)

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
        <> T.pack (Prelude.show invalid)

data Protocol = MockedBFT
  deriving (Eq, Show)

getConfiguration :: FilePath -> NodeConfiguration Maybe -> IO (NodeConfiguration Identity)
getConfiguration file cliConfig = do
  value <- decodeFileThrow file
  case parse parser value of
    Error err -> fail err
    Success config -> return config
  where
    parser =
      withObject "NodeConfiguration" $ \v ->
        bsequence' $ bzipWith3 combine cliConfig (parseConfigFile v) defaultConfiguration
    combine :: Maybe a -> Parser a -> Maybe a -> Parser a
    combine (Just v) _ _ = return v
    combine _ p Nothing = p
    combine _ p (Just def) = parserCatchError p (\_ _ -> return def)

data MiscellaneousFilepaths = MiscellaneousFilepaths
  { topFile :: !TopologyFile,
    dBFile :: !DbFile,
    socketFile :: !SocketFile
  }
  deriving (Show)

newtype TopologyFile = TopologyFile
  {unTopology :: FilePath}
  deriving (Show)

newtype DbFile = DbFile
  {unDB :: FilePath}
  deriving (Show)

newtype GenesisFile = GenesisFile
  {unGenesisFile :: FilePath}
  deriving (Eq, Ord, Show, IsString)

newtype DelegationCertFile = DelegationCertFile
  {unDelegationCert :: FilePath}
  deriving (Show)

newtype SocketFile = SocketFile
  {unSocket :: FilePath}
  deriving (Show)

newtype SigningKeyFile = SigningKeyFile
  {unSigningKey :: FilePath}
  deriving (Eq, Ord, Show, IsString)

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case unNodeHostAddress addr of
    Just (IP.IPv4 ipv4) -> SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing -> SockAddrInet port 0 -- Could also be any IPv6 addr

-- | Detailed tracing options. Each option enables a tracer
--   which verbosity to the log output.
data TraceOptions = TraceOptions
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
    traceHandshake :: Bool,
    traceLedgerState :: !Bool,
    tracePoWNodeRpc :: !Bool,
    traceTimeTravelError :: !Bool
  }
  deriving (Eq, Show)

newtype ConfigYamlFilePath = ConfigYamlFilePath
  {unConfigPath :: FilePath}
  deriving (Show)

-- | IPv4 address with a port number.
data NodeAddress = NodeAddress
  { naHostAddress :: !NodeHostAddress,
    naPort :: !PortNumber
  }
  deriving (Eq, Ord, Show)

newtype NodeHostAddress = NodeHostAddress {unNodeHostAddress :: Maybe IP.IP}
  deriving (Eq, Ord, Show)
