{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Types
  ( ConfigYamlFilePath (..),
    DbFile (..),
    DelegationCertFile (..),
    GenesisFile (..),
    MiscellaneousFilepaths (..),
    NodeConfiguration_ (..),
    NodeConfiguration,
    Protocol (..),
    SigningKeyFile (..),
    SocketFile (..),
    TopologyFile (..),
    TraceOptions_ (..),
    TraceOptions,
    NodeAddress (..),
    NodeHostAddress (..),
    nodeAddressToSockAddr,
    getConfiguration,
    emptyConfiguration,
  )
where

import Barbies
import Barbies.Bare
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

data NodeConfiguration_ w f = NodeConfiguration
  { ncProtocol :: Wear w f Protocol,
    ncNodeId :: Wear w f CoreNodeId,
    ncNumCoreNodes :: Wear w f Word64,
    ncNetworkMagic :: Wear w f Word32,
    ncSystemStart :: Wear w f (Maybe SystemStart),
    ncSecurityParameter :: Wear w f Word64,
    ncStableLedgerDepth :: Wear w f Int,
    ncLoggingSwitch :: Wear w f Bool,
    ncTraceOpts :: TraceOptions_ w f,
    ncTimeslotLength :: Wear w f SlotLength,
    ncSnapshotsOnDisk :: Wear w f Int,
    ncSnapshotInterval :: Wear w f Word64,
    ncPoWBlockFetchInterval :: Wear w f Int,
    ncPoWNodeRpcUrl :: Wear w f Text,
    ncPrometheusPort :: Wear w f Int,
    -- FIXME: separate data type: CheckpointingConfiguration
    ncCheckpointInterval :: Wear w f Int,
    ncRequiredMajority :: Wear w f Int,
    ncFedPubKeys :: Wear w f [PublicKey],
    ncNodePrivKeyFile :: Wear w f FilePath,
    ncTopologyFile :: Wear w f TopologyFile,
    ncDatabaseDir :: Wear w f DbFile,
    ncSocketFile :: Wear w f SocketFile,
    ncNodeHost :: Wear w f NodeHostAddress,
    ncNodePort :: Wear w f PortNumber,
    ncValidateDb :: Wear w f Bool
  }
  deriving (Generic)

instance FunctorB (NodeConfiguration_ Covered)

instance ApplicativeB (NodeConfiguration_ Covered)

instance TraversableB (NodeConfiguration_ Covered)

instance ConstraintsB (NodeConfiguration_ Covered)

deriving instance AllBF Eq f (NodeConfiguration_ Covered) => Eq (NodeConfiguration_ Covered f)

deriving instance AllBF Show f (NodeConfiguration_ Covered) => Show (NodeConfiguration_ Covered f)

instance BareB NodeConfiguration_

instance FunctorB (NodeConfiguration_ Bare)

instance ConstraintsB (NodeConfiguration_ Bare)

deriving instance AllBF Eq f (NodeConfiguration_ Bare) => Eq (NodeConfiguration_ Bare f)

deriving instance AllBF Show f (NodeConfiguration_ Bare) => Show (NodeConfiguration_ Bare f)

type NodeConfiguration = NodeConfiguration_ Bare Identity

parseConfigFile :: Object -> NodeConfiguration_ Covered Parser
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

emptyConfiguration :: NodeConfiguration_ Covered Maybe
emptyConfiguration = bpure Nothing

defaultConfiguration :: NodeConfiguration_ Covered Maybe
defaultConfiguration =
  emptyConfiguration
    { ncSystemStart = Just Nothing,
      ncLoggingSwitch = Just True,
      ncSnapshotsOnDisk = Just 60,
      ncSnapshotInterval = Just 60,
      ncPoWBlockFetchInterval = Just 1000000,
      ncTraceOpts = bcoverWith Just defaultTraceOptions,
      ncNodeHost = Just (NodeHostAddress Nothing),
      ncValidateDb = Just False
    }

instance FromJSON SystemStart where
  parseJSON v = SystemStart <$> parseJSON v

defaultTraceOptions :: TraceOptions
defaultTraceOptions =
  TraceOptions
    { traceVerbosity = NormalVerbosity,
      traceChainDB = True,
      traceChainSyncClient = True,
      traceChainSyncHeaderServer = True,
      traceChainSyncBlockServer = True,
      traceBlockFetchDecisions = True,
      traceBlockFetchClient = True,
      traceBlockFetchServer = True,
      traceTxInbound = True,
      traceTxOutbound = True,
      traceLocalTxSubmissionServer = True,
      traceMempool = True,
      traceForge = True,
      traceChainSyncProtocol = True,
      traceBlockFetchProtocol = True,
      traceBlockFetchProtocolSerialised = True,
      traceTxSubmissionProtocol = True,
      traceLocalChainSyncProtocol = True,
      traceLocalTxSubmissionProtocol = True,
      traceLocalStateQueryProtocol = True,
      traceIpSubscription = True,
      traceDnsSubscription = True,
      traceDnsResolver = True,
      traceErrorPolicy = True,
      traceMux = True,
      traceHandshake = True,
      traceLedgerState = True,
      tracePoWNodeRpc = True,
      traceTimeTravelError = True
    }

traceConfigParser :: Object -> TraceOptions_ Covered Parser
traceConfigParser v =
  TraceOptions
    { traceVerbosity = v .: "TracingVerbosity",
      traceChainDB = v .: "TraceChainDb",
      traceChainSyncClient = v .: "TraceChainSyncClient",
      traceChainSyncHeaderServer = v .: "TraceChainSyncHeaderServer",
      traceChainSyncBlockServer = v .: "TraceChainSyncBlockServer",
      traceBlockFetchDecisions = v .: "TraceBlockFetchDecisions",
      traceBlockFetchClient = v .: "TraceBlockFetchServer",
      traceBlockFetchServer = v .: "TraceBlockFetchClient",
      traceTxInbound = v .: "TraceTxInbound",
      traceTxOutbound = v .: "TraceTxOutbound",
      traceLocalTxSubmissionServer = v .: "TraceLocalTxSubmissionServer",
      traceMempool = v .: "TraceMempool",
      traceForge = v .: "TraceForge",
      traceChainSyncProtocol = v .: "TraceChainSyncProtocol",
      traceBlockFetchProtocol = v .: "TraceBlockFetchProtocol",
      traceBlockFetchProtocolSerialised = v .: "TraceBlockFetchProtocolSerialised",
      traceTxSubmissionProtocol = v .: "TraceTxSubmissionProtocol",
      traceLocalChainSyncProtocol = v .: "TraceLocalChainSyncProtocol",
      traceLocalTxSubmissionProtocol = v .: "TraceLocalTxSubmissionProtocol",
      traceLocalStateQueryProtocol = v .: "traceLocalStateQueryProtocol",
      traceIpSubscription = v .: "TraceIpSubscription",
      traceDnsSubscription = v .: "TraceDNSSubscription",
      traceDnsResolver = v .: "TraceDNSResolver",
      traceErrorPolicy = v .: "TraceErrorPolicy",
      traceMux = v .: "TraceMux",
      traceHandshake = v .: "TraceHandshake",
      traceLedgerState = v .: "TraceLedgerState",
      tracePoWNodeRpc = v .: "TracePoWNodeRpc",
      traceTimeTravelError = v .: "TraceTimeTravelError"
    }

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

getConfiguration :: FilePath -> NodeConfiguration_ Covered Maybe -> IO NodeConfiguration
getConfiguration file cliConfig = do
  value <- decodeFileThrow file
  case parse parser value of
    Error err -> fail err
    Success config -> return $ bstrip config
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
  deriving (Eq, Show)

newtype DbFile = DbFile
  {unDB :: FilePath}
  deriving (Eq, Show)

newtype GenesisFile = GenesisFile
  {unGenesisFile :: FilePath}
  deriving (Eq, Ord, Show, IsString)

newtype DelegationCertFile = DelegationCertFile
  {unDelegationCert :: FilePath}
  deriving (Show)

newtype SocketFile = SocketFile
  {unSocket :: FilePath}
  deriving (Eq, Show)

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
data TraceOptions_ w f = TraceOptions
  { traceVerbosity :: Wear w f TracingVerbosity,
    -- | By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer
    traceChainDB :: Wear w f Bool,
    -- Consensus Tracers --
    traceChainSyncClient :: Wear w f Bool,
    traceChainSyncHeaderServer :: Wear w f Bool,
    traceChainSyncBlockServer :: Wear w f Bool,
    traceBlockFetchDecisions :: Wear w f Bool,
    traceBlockFetchClient :: Wear w f Bool,
    traceBlockFetchServer :: Wear w f Bool,
    traceTxInbound :: Wear w f Bool,
    traceTxOutbound :: Wear w f Bool,
    traceLocalTxSubmissionServer :: Wear w f Bool,
    traceMempool :: Wear w f Bool,
    traceForge :: Wear w f Bool,
    -----------------------

    -- Protocol Tracers --
    traceChainSyncProtocol :: Wear w f Bool,
    -- There's two variants of the block fetch tracer and for now
    -- at least we'll set them both together from the same flags.
    traceBlockFetchProtocol :: Wear w f Bool,
    traceBlockFetchProtocolSerialised :: Wear w f Bool,
    traceTxSubmissionProtocol :: Wear w f Bool,
    traceLocalChainSyncProtocol :: Wear w f Bool,
    traceLocalTxSubmissionProtocol :: Wear w f Bool,
    traceLocalStateQueryProtocol :: Wear w f Bool,
    traceIpSubscription :: Wear w f Bool,
    -----------------------

    traceDnsSubscription :: Wear w f Bool,
    traceDnsResolver :: Wear w f Bool,
    traceErrorPolicy :: Wear w f Bool,
    traceMux :: Wear w f Bool,
    traceHandshake :: Wear w f Bool,
    traceLedgerState :: Wear w f Bool,
    tracePoWNodeRpc :: Wear w f Bool,
    traceTimeTravelError :: Wear w f Bool
  }
  deriving (Generic)

instance FunctorB (TraceOptions_ Covered)

instance ApplicativeB (TraceOptions_ Covered)

instance TraversableB (TraceOptions_ Covered)

instance ConstraintsB (TraceOptions_ Covered)

deriving instance AllBF Eq f (TraceOptions_ Covered) => Eq (TraceOptions_ Covered f)

deriving instance AllBF Show f (TraceOptions_ Covered) => Show (TraceOptions_ Covered f)

instance BareB TraceOptions_

instance FunctorB (TraceOptions_ Bare)

instance ConstraintsB (TraceOptions_ Bare)

deriving instance AllBF Eq f (TraceOptions_ Bare) => Eq (TraceOptions_ Bare f)

deriving instance AllBF Show f (TraceOptions_ Bare) => Show (TraceOptions_ Bare f)

type TraceOptions = TraceOptions_ Bare Identity

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
