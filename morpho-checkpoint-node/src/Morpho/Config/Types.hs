{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Types
  ( ConfigYamlFilePath (..),
    DbFile (..),
    DelegationCertFile (..),
    GenesisFile (..),
    MiscellaneousFilepaths (..),
    NodeCLI (..),
    NodeConfiguration,
    NodeConfigurationPartial,
    NodeConfiguration_ (..),
    Protocol (..),
    SigningKeyFile (..),
    SocketFile (..),
    TopologyFile (..),
    TraceOptions,
    TraceOptionsPartial,
    TraceOptions_ (..),
    NodeAddress (..),
    NodeHostAddress (..),
    nodeAddressToSockAddr,
    getNodeConfiguration,
  )
where

import Barbies
import Barbies.Bare
import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Cardano.Prelude
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Functor.Compose
import qualified Data.IP as IP
import qualified Data.Text as T
import Data.Time
import Data.Yaml (decodeFileThrow)
import Morpho.Config.Orphans ()
import Morpho.Crypto.ECDSASignature
import Network.Socket
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import qualified Prelude

data NodeConfiguration_ t f = NodeConfiguration
  { ncProtocol :: Wear t f Protocol,
    ncNodeId :: Wear t f CoreNodeId,
    ncNumCoreNodes :: Wear t f Word64,
    ncReqNetworkMagic :: Wear t f RequiresNetworkMagic,
    ncNetworkMagic :: Wear t f Word32,
    ncSystemStart :: Wear t f SystemStart,
    ncSecurityParameter :: Wear t f Word64,
    ncStableLedgerDepth :: Wear t f Int,
    ncLoggingSwitch :: Wear t f Bool,
    ncTraceOpts :: TraceOptions_ t f,
    ncLogMetrics :: Wear t f Bool,
    ncTimeslotLength :: Wear t f SlotLength,
    ncSnapshotsOnDisk :: Wear t f Int,
    ncSnapshotInterval :: Wear t f Word64,
    ncPoWBlockFetchInterval :: Wear t f Int,
    ncPoWNodeRpcUrl :: Wear t f Text,
    ncPrometheusPort :: Wear t f Int,
    -- FIXME: separate data type: CheckpointingConfiguration
    ncCheckpointInterval :: Wear t f Int,
    ncRequiredMajority :: Wear t f Int,
    ncFedPubKeys :: Wear t f [PublicKey],
    ncNodePrivKeyFile :: Wear t f FilePath,
    ncValidateDB :: Wear t f Bool
  }
  deriving (Generic)

instance FunctorB (NodeConfiguration_ Covered)

instance TraversableB (NodeConfiguration_ Covered)

instance ApplicativeB (NodeConfiguration_ Covered)

instance ConstraintsB (NodeConfiguration_ Covered)

instance BareB NodeConfiguration_

instance (Alternative f) => Semigroup (NodeConfiguration_ Covered f) where
  (<>) = bzipWith (<|>)

deriving instance AllBF Show f (NodeConfiguration_ Covered) => Show (NodeConfiguration_ Covered f)

deriving instance AllBF Eq f (NodeConfiguration_ Covered) => Eq (NodeConfiguration_ Covered f)

type NodeConfiguration = NodeConfiguration_ Bare Identity

type NodeConfigurationPartial = NodeConfiguration_ Covered Maybe

instance FromJSON NodeConfigurationPartial where
  parseJSON = withObject "NodeConfiguration" $ \v -> do
    nId <- v .: "NodeId"
    ptcl <- v .: "Protocol"
    numCoreNode <- v .: "NumCoreNodes"
    rNetworkMagic <- v .: "RequiresNetworkMagic"
    networkMagic <- v .: "NetworkMagic"
    systemStart <- v .:? "SystemStart"
    securityParam <- v .: "SecurityParam"
    stableLedgerDepth <- v .: "StableLedgerDepth"
    loggingSwitch <- v .: "TurnOnLogging"
    traceOptions <- traceConfigParser v
    logMetrics <- v .: "TurnOnLogMetrics"
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
        (CoreNodeId <$> nId)
        numCoreNode
        rNetworkMagic
        networkMagic
        systemStart
        securityParam
        stableLedgerDepth
        loggingSwitch
        traceOptions
        logMetrics
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
        Nothing

defaultNodeConfiguration :: IO NodeConfigurationPartial
defaultNodeConfiguration =
  bsequence $
    (bpure (Compose $ return Nothing))
      { ncSystemStart =
          Compose $
            Just . SystemStart <$> getCurrentTime,
        ncTraceOpts = bmap (Compose . return) $ bcoverWith Just defaultTraceOptions,
        ncPoWBlockFetchInterval = Compose $ return $ Just 1000000
      }

cliToNodeConfiguration :: NodeCLI -> NodeConfigurationPartial
cliToNodeConfiguration nCli =
  (bpure Nothing)
    { ncValidateDB = Just $ validateDB nCli
    }

instance FromJSON SystemStart where
  parseJSON v = SystemStart <$> parseJSON v

traceConfigParser :: Object -> Parser TraceOptionsPartial
traceConfigParser v =
  TraceOptions
    <$> v .:? "TracingVerbosity"
    <*> v .:? "TraceChainDb"
    <*> v .:? "TraceChainSyncClient"
    <*> v .:? "TraceChainSyncHeaderServer"
    <*> v .:? "TraceChainSyncBlockServer"
    <*> v .:? "TraceBlockFetchDecisions"
    <*> v .:? "TraceBlockFetchServer"
    <*> v .:? "TraceBlockFetchClient"
    <*> v .:? "TraceTxInbound"
    <*> v .:? "TraceTxOutbound"
    <*> v .:? "TraceLocalTxSubmissionServer"
    <*> v .:? "TraceMempool"
    <*> v .:? "TraceForge"
    <*> v .:? "TraceChainSyncProtocol"
    <*> v .:? "TraceBlockFetchProtocol"
    <*> v .:? "TraceBlockFetchProtocolSerialised"
    <*> v .:? "TraceTxSubmissionProtocol"
    <*> v .:? "TraceLocalChainSyncProtocol"
    <*> v .:? "TraceLocalTxSubmissionProtocol"
    <*> v .:? "traceLocalStateQueryProtocol"
    <*> v .:? "TraceIpSubscription"
    <*> v .:? "TraceDNSSubscription"
    <*> v .:? "TraceDNSResolver"
    <*> v .:? "TraceErrorPolicy"
    <*> v .:? "TraceMux"
    <*> v .:? "TraceHandshake"
    <*> v .:? "TraceLedgerState"
    <*> v .:? "TracePoWNodeRpc"
    <*> v .:? "TraceTimeTravelError"

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

getNodeConfiguration :: NodeCLI -> FilePath -> IO (Maybe NodeConfiguration)
getNodeConfiguration nCli file = do
  let fromCLI = cliToNodeConfiguration nCli
  fromFile <- decodeFileThrow file
  fromDefaults <- defaultNodeConfiguration
  let combined = fromCLI <> fromFile <> fromDefaults
  return $ bstrip <$> bsequence' combined

data NodeCLI = NodeCLI
  { mscFp :: !MiscellaneousFilepaths,
    nodeAddr :: !NodeAddress,
    configFp :: !ConfigYamlFilePath,
    validateDB :: !Bool
  }
  deriving (Show)

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
  deriving (Eq, Ord, Show)
  deriving newtype (IsString)

newtype DelegationCertFile = DelegationCertFile
  {unDelegationCert :: FilePath}
  deriving (Show)

newtype SocketFile = SocketFile
  {unSocket :: FilePath}
  deriving (Show)

newtype SigningKeyFile = SigningKeyFile
  {unSigningKey :: FilePath}
  deriving (Eq, Ord, Show)
  deriving newtype (IsString)

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case unNodeHostAddress addr of
    Just (IP.IPv4 ipv4) -> SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing -> SockAddrInet port 0 -- Could also be any IPv6 addr

-- | Detailed tracing options. Each option enables a tracer
--   which verbosity to the log output.
data TraceOptions_ t f = TraceOptions
  { traceVerbosity :: Wear t f TracingVerbosity,
    -- | By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer
    traceChainDB :: Wear t f Bool,
    -- Consensus Tracers --
    traceChainSyncClient :: Wear t f Bool,
    traceChainSyncHeaderServer :: Wear t f Bool,
    traceChainSyncBlockServer :: Wear t f Bool,
    traceBlockFetchDecisions :: Wear t f Bool,
    traceBlockFetchClient :: Wear t f Bool,
    traceBlockFetchServer :: Wear t f Bool,
    traceTxInbound :: Wear t f Bool,
    traceTxOutbound :: Wear t f Bool,
    traceLocalTxSubmissionServer :: Wear t f Bool,
    traceMempool :: Wear t f Bool,
    traceForge :: Wear t f Bool,
    -----------------------

    -- Protocol Tracers --
    traceChainSyncProtocol :: Wear t f Bool,
    -- There's two variants of the block fetch tracer and for now
    -- at least we'll set them both together from the same flags.
    traceBlockFetchProtocol :: Wear t f Bool,
    traceBlockFetchProtocolSerialised :: Wear t f Bool,
    traceTxSubmissionProtocol :: Wear t f Bool,
    traceLocalChainSyncProtocol :: Wear t f Bool,
    traceLocalTxSubmissionProtocol :: Wear t f Bool,
    traceLocalStateQueryProtocol :: Wear t f Bool,
    traceIpSubscription :: Wear t f Bool,
    -----------------------

    traceDnsSubscription :: Wear t f Bool,
    traceDnsResolver :: Wear t f Bool,
    traceErrorPolicy :: Wear t f Bool,
    traceMux :: Wear t f Bool,
    traceHandshake :: Wear t f Bool,
    traceLedgerState :: Wear t f Bool,
    tracePoWNodeRpc :: Wear t f Bool,
    traceTimeTravelError :: Wear t f Bool
  }
  deriving (Generic)

instance FunctorB (TraceOptions_ Covered)

instance TraversableB (TraceOptions_ Covered)

instance ApplicativeB (TraceOptions_ Covered)

instance ConstraintsB (TraceOptions_ Covered)

instance BareB TraceOptions_

deriving instance AllBF Show f (TraceOptions_ Covered) => Show (TraceOptions_ Covered f)

deriving instance AllBF Eq f (TraceOptions_ Covered) => Eq (TraceOptions_ Covered f)

type TraceOptions = TraceOptions_ Bare Identity

type TraceOptionsPartial = TraceOptions_ Covered Maybe

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
