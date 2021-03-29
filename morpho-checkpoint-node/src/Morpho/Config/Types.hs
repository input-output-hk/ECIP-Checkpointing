{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Types
  ( ConfigYamlFilePath (..),
    DbFile (..),
    NodeConfiguration_ (..),
    NodeConfiguration,
    NodeConfigurationFunctor,
    Protocol (..),
    TopologyFile (..),
    TraceOptions_ (..),
    TraceOptions,
    TraceOptionsFunctor,
    NodeAddress (..),
    NodeHostAddress (..),
    nodeAddressToSockAddr,
    configFieldName,
    configFieldParser,
    configFieldDefault,
  )
where

import Barbies
import Barbies.Bare
import Cardano.BM.Data.Configuration
import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Prelude
import Data.Aeson
import Data.Functor.Compose
import qualified Data.IP as IP
import qualified Data.Text as T
import Data.Time ()
import Morpho.Config.Orphans ()
import Morpho.Crypto.ECDSASignature
import Network.Socket
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import qualified Prelude

-- | For configuration, we use the approach described by
-- <https://chrispenner.ca/posts/hkd-options>, making heavy use of the barbies
-- library.
-- See <https://hackage.haskell.org/package/barbies-2.0.2.0/docs/Barbies.html>
-- for an introduction
-- See <https://hackage.haskell.org/package/barbies-2.0.2.0/docs/Barbies-Bare.html>
-- to see what's up with the 'Wear' thing
data NodeConfiguration_ w f = NodeConfiguration
  { ncProtocol :: Wear w f Protocol,
    ncNodeId :: Wear w f CoreNodeId,
    ncNumCoreNodes :: Wear w f Word64,
    ncNetworkMagic :: Wear w f Word32,
    ncSystemStart :: Wear w f SystemStart,
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
    ncNodeHost :: Wear w f NodeHostAddress,
    ncNodePort :: Wear w f PortNumber,
    ncValidateDb :: Wear w f Bool,
    ncLogging :: Wear w f Representation
  }
  deriving (Generic)

-- | A convenience type alias for a 'NodeConfiguration' where every field is
-- covered with a functor
type NodeConfigurationFunctor = NodeConfiguration_ Covered

-- | A convenience type alias for a 'NodeConfiguration' where every field is
-- bare, not covered by any functor
type NodeConfiguration = NodeConfiguration_ Bare Identity

instance FunctorB NodeConfigurationFunctor

instance ApplicativeB NodeConfigurationFunctor

instance TraversableB NodeConfigurationFunctor

instance ConstraintsB NodeConfigurationFunctor

deriving instance AllBF Eq f NodeConfigurationFunctor => Eq (NodeConfigurationFunctor f)

deriving instance AllBF Show f NodeConfigurationFunctor => Show (NodeConfigurationFunctor f)

instance BareB NodeConfiguration_

instance FunctorB (NodeConfiguration_ Bare)

instance ConstraintsB (NodeConfiguration_ Bare)

deriving instance AllBF Eq f (NodeConfiguration_ Bare) => Eq (NodeConfiguration_ Bare f)

deriving instance AllBF Show f (NodeConfiguration_ Bare) => Show (NodeConfiguration_ Bare f)

-- | Determines the mapping from configuration file field to 'NodeConfiguration'
configFieldName :: NodeConfigurationFunctor (Const Text)
configFieldName =
  NodeConfiguration
    { ncProtocol = "Protocol",
      ncNodeId = "NodeId",
      ncNumCoreNodes = "NumCoreNodes",
      ncNetworkMagic = "NetworkMagic",
      ncSystemStart = "SystemStart",
      ncSecurityParameter = "SecurityParam",
      ncStableLedgerDepth = "StableLedgerDepth",
      ncLoggingSwitch = "TurnOnLogging",
      ncTraceOpts =
        TraceOptions
          { traceVerbosity = "TracingVerbosity",
            traceChainDB = "TraceChainDb",
            traceChainSyncClient = "TraceChainSyncClient",
            traceChainSyncHeaderServer = "TraceChainSyncHeaderServer",
            traceChainSyncBlockServer = "TraceChainSyncBlockServer",
            traceBlockFetchDecisions = "TraceBlockFetchDecisions",
            traceBlockFetchClient = "TraceBlockFetchServer",
            traceBlockFetchServer = "TraceBlockFetchClient",
            traceTxInbound = "TraceTxInbound",
            traceTxOutbound = "TraceTxOutbound",
            traceLocalTxSubmissionServer = "TraceLocalTxSubmissionServer",
            traceMempool = "TraceMempool",
            traceForge = "TraceForge",
            traceChainSyncProtocol = "TraceChainSyncProtocol",
            traceBlockFetchProtocol = "TraceBlockFetchProtocol",
            traceBlockFetchProtocolSerialised = "TraceBlockFetchProtocolSerialised",
            traceTxSubmissionProtocol = "TraceTxSubmissionProtocol",
            traceLocalChainSyncProtocol = "TraceLocalChainSyncProtocol",
            traceLocalTxSubmissionProtocol = "TraceLocalTxSubmissionProtocol",
            traceLocalStateQueryProtocol = "traceLocalStateQueryProtocol",
            traceIpSubscription = "TraceIpSubscription",
            traceDnsSubscription = "TraceDNSSubscription",
            traceDnsResolver = "TraceDNSResolver",
            traceErrorPolicy = "TraceErrorPolicy",
            traceMux = "TraceMux",
            traceHandshake = "TraceHandshake",
            traceLedgerState = "TraceLedgerState",
            tracePoWNodeRpc = "TracePoWNodeRpc",
            traceTimeTravelError = "TraceTimeTravelError"
          },
      ncTimeslotLength = "SlotDuration",
      ncSnapshotsOnDisk = "SnapshotsOnDisk",
      ncSnapshotInterval = "SnapshotInterval",
      ncPoWBlockFetchInterval = "PoWBlockFetchInterval",
      ncPoWNodeRpcUrl = "PoWNodeRpcUrl",
      ncPrometheusPort = "PrometheusPort",
      ncCheckpointInterval = "CheckpointInterval",
      ncRequiredMajority = "RequiredMajority",
      ncFedPubKeys = "FedPubKeys",
      ncNodePrivKeyFile = "NodePrivKeyFile",
      ncTopologyFile = "TopologyFile",
      ncDatabaseDir = "DatabaseDirectory",
      ncNodeHost = "NodeHost",
      ncNodePort = "NodePort",
      ncValidateDb = "ValidateDatabase",
      ncLogging = "Logging"
    }

-- | Determines how each configuration field should be parsed
-- The type argument @(->) Value `Compose` Result@ means that each field has
-- type @Value -> Result a@, which given a JSON 'Value' of the field, returns
-- a 'Result' containing the parsed value
configFieldParser :: NodeConfigurationFunctor ((->) Value `Compose` Result)
configFieldParser =
  -- TODO: Change the NodeConfiguration type to something where each field
  -- can be fromJSON'd directly. Then try to derive all of these values to
  -- 'Compose fromJSON'
  NodeConfiguration
    { ncProtocol = Compose fromJSON,
      ncNodeId = CoreNodeId <$> Compose fromJSON,
      ncNumCoreNodes = Compose fromJSON,
      ncNetworkMagic = Compose fromJSON,
      ncSystemStart = Compose fromJSON,
      ncSecurityParameter = Compose fromJSON,
      ncStableLedgerDepth = Compose fromJSON,
      ncLoggingSwitch = Compose fromJSON,
      ncTraceOpts =
        TraceOptions
          { traceVerbosity = Compose fromJSON,
            traceChainDB = Compose fromJSON,
            traceChainSyncClient = Compose fromJSON,
            traceChainSyncHeaderServer = Compose fromJSON,
            traceChainSyncBlockServer = Compose fromJSON,
            traceBlockFetchDecisions = Compose fromJSON,
            traceBlockFetchClient = Compose fromJSON,
            traceBlockFetchServer = Compose fromJSON,
            traceTxInbound = Compose fromJSON,
            traceTxOutbound = Compose fromJSON,
            traceLocalTxSubmissionServer = Compose fromJSON,
            traceMempool = Compose fromJSON,
            traceForge = Compose fromJSON,
            traceChainSyncProtocol = Compose fromJSON,
            traceBlockFetchProtocol = Compose fromJSON,
            traceBlockFetchProtocolSerialised = Compose fromJSON,
            traceTxSubmissionProtocol = Compose fromJSON,
            traceLocalChainSyncProtocol = Compose fromJSON,
            traceLocalTxSubmissionProtocol = Compose fromJSON,
            traceLocalStateQueryProtocol = Compose fromJSON,
            traceIpSubscription = Compose fromJSON,
            traceDnsSubscription = Compose fromJSON,
            traceDnsResolver = Compose fromJSON,
            traceErrorPolicy = Compose fromJSON,
            traceMux = Compose fromJSON,
            traceHandshake = Compose fromJSON,
            traceLedgerState = Compose fromJSON,
            tracePoWNodeRpc = Compose fromJSON,
            traceTimeTravelError = Compose fromJSON
          },
      ncTimeslotLength = Compose fromJSON,
      ncSnapshotsOnDisk = Compose fromJSON,
      ncSnapshotInterval = Compose fromJSON,
      ncPoWBlockFetchInterval = Compose fromJSON,
      ncPoWNodeRpcUrl = Compose fromJSON,
      ncPrometheusPort = Compose fromJSON,
      ncCheckpointInterval = Compose fromJSON,
      ncRequiredMajority = Compose fromJSON,
      ncFedPubKeys = Compose fromJSON,
      ncNodePrivKeyFile = Compose fromJSON,
      ncTopologyFile = TopologyFile <$> Compose fromJSON,
      ncDatabaseDir = DbFile <$> Compose fromJSON,
      ncNodeHost = NodeHostAddress . readMaybe . T.unpack <$> Compose fromJSON,
      ncNodePort = (fromIntegral :: Int -> PortNumber) <$> Compose fromJSON,
      ncValidateDb = Compose fromJSON,
      ncLogging = Compose fromJSON
    }

-- | The configuration defaults for each field. Gets overridden by CLI and
-- config file
configFieldDefault :: NodeConfigurationFunctor Maybe
configFieldDefault =
  (bpure Nothing)
    { ncLoggingSwitch = Just True,
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

newtype TopologyFile = TopologyFile
  {unTopology :: FilePath}
  deriving (Eq, Show)

newtype DbFile = DbFile
  {unDB :: FilePath}
  deriving (Eq, Show)

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

-- A convenience type alias for a TraceOptions where every field is
-- covered with a functor
type TraceOptionsFunctor = TraceOptions_ Covered

-- A convenience type alias for a TraceOptions where every field is
-- bare, not covered by any functor
type TraceOptions = TraceOptions_ Bare Identity

instance FunctorB TraceOptionsFunctor

instance ApplicativeB TraceOptionsFunctor

instance TraversableB TraceOptionsFunctor

instance ConstraintsB TraceOptionsFunctor

deriving instance AllBF Eq f TraceOptionsFunctor => Eq (TraceOptionsFunctor f)

deriving instance AllBF Show f TraceOptionsFunctor => Show (TraceOptionsFunctor f)

instance BareB TraceOptions_

instance FunctorB (TraceOptions_ Bare)

instance ConstraintsB (TraceOptions_ Bare)

deriving instance AllBF Eq f (TraceOptions_ Bare) => Eq (TraceOptions_ Bare f)

deriving instance AllBF Show f (TraceOptions_ Bare) => Show (TraceOptions_ Bare f)

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
