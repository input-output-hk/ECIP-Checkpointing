{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Blockchain.Config.Types (
  ConfigYamlFilePath(..),
  DbFile(..),
  DelegationCertFile(..),
  GenesisFile(..),
  MiscellaneousFilepaths(..),
  NodeCLI (..),
  NodeConfiguration (..),
  Protocol(..),
  SigningKeyFile(..),
  SocketFile(..),
  TopologyFile(..),
  TraceOptions (..),
  Update(..),
  nodeAddressInfo,
  nodeAddressToSockAddr,
  parseNodeConfiguration
) where

import Cardano.Prelude
import qualified Prelude

import Blockchain.Config.Orphans ()
import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Data.Aeson
import Data.Yaml (decodeFileThrow)
import Network.Socket
import Ouroboros.Consensus.NodeId (NodeId(..))
import Ouroboros.Consensus.BlockchainTime
import qualified Cardano.Chain.Update as Update
import qualified Data.Text as T
import qualified Data.IP as IP

import Blockchain.Config.Topology
import Blockchain.Crypto.ECDSASignature

data NodeConfiguration =
    NodeConfiguration
      { ncProtocol :: Protocol
      , ncNodeId :: NodeId
      , ncNumCoreNodes :: Int
      , ncReqNetworkMagic :: RequiresNetworkMagic
      , ncPbftSignatureThresh :: Maybe Double
      , ncLoggingSwitch :: Bool
      , ncLogMetrics :: Bool
      , ncViewMode :: ViewMode
      , ncUpdate :: Update
      , ncTimeslotLength :: SlotLength
      , ncSnapshotsOnDisk :: Int
      , ncSnapshotInterval :: Int
      , ncBlockchainBlockFetchInterval :: Maybe Int
      , ncBlockchainRpcUrl :: Text
      , ncPrometheusPort :: Int
      -- FIXME: separate data type: CheckpointingConfiguration
      , ncCheckpointInterval :: Int
      , ncRequiredMajority :: Int
      , ncFedPubKeys :: [PublicKey]
      , ncNodePrivKeyFile :: FilePath
      } deriving (Show)

instance FromJSON NodeConfiguration where
    parseJSON = withObject "NodeConfiguration" $ \v -> do
                  nId <- v .: "NodeId"
                  ptcl <- v .: "Protocol"
                  numCoreNode <- v .: "NumCoreNodes"
                  rNetworkMagic <- v .: "RequiresNetworkMagic"
                  pbftSignatureThresh <- v .:? "PBftSignatureThreshold"
                  loggingSwitch <- v .: "TurnOnLogging"
                  vMode <- v .: "ViewMode"
                  logMetrics <- v .: "TurnOnLogMetrics"

                  -- Update Parameters
                  appName <- v .: "ApplicationName"
                  appVersion <- v .: "ApplicationVersion"
                  lkBlkVersionMajor <- v .: "LastKnownBlockVersion-Major"
                  lkBlkVersionMinor <- v .: "LastKnownBlockVersion-Minor"
                  lkBlkVersionAlt <- v .: "LastKnownBlockVersion-Alt"
                  slotLength <- v .: "SlotDuration"
                  snapshotsOnDisk <- v .: "SnapshotsOnDisk"
                  snapshotInterval <- v .: "SnapshotInterval"
                  blockFetchInterval <- v .:? "BlockchainBlockFetchInterval"
                  blockchainRpcUrl <- v .: "BlockchainRpcUrl"
                  promPort <- v .: "PrometheusPort"


                  -- Checkpointing parameters
                  checkpointInterval <- v .: "CheckpointInterval"
                  requiredMajority <- v .: "RequiredMajority"
                  fedPubKeys <- v .: "FedPubKeys"
                  nodePrivKeyFile <- v .: "NodePrivKeyFile"

                  pure $ NodeConfiguration
                           ptcl
                           nId
                           numCoreNode
                           rNetworkMagic
                           pbftSignatureThresh
                           loggingSwitch
                           logMetrics
                           vMode
                           (Update appName appVersion (LastKnownBlockVersion
                                                         lkBlkVersionMajor
                                                         lkBlkVersionMinor
                                                         lkBlkVersionAlt))
                           slotLength
                           snapshotsOnDisk
                           snapshotInterval
                           blockFetchInterval
                           blockchainRpcUrl
                           promPort
                           checkpointInterval
                           requiredMajority
                           fedPubKeys
                           nodePrivKeyFile


instance FromJSON Protocol where
  parseJSON (String str) = case str of
                            "MockedBFT" -> pure MockedBFT
                            ptcl -> panic $ "Parsing of Protocol: "
                                          <> ptcl <> " failed. "
                                          <> ptcl <> " is not a valid protocol"
  parseJSON invalid  = panic $ "Parsing of Protocol failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)


instance FromJSON ViewMode where
  parseJSON (String str) = case str of
                            "LiveView" -> pure LiveView
                            "SimpleView" -> pure SimpleView
                            view -> panic $ "Parsing of ViewMode: "
                                          <> view <> " failed. "
                                          <> view <> " is not a valid view mode"
  parseJSON invalid  = panic $ "Parsing of ViewMode failed due to type mismatch. "
                             <> "Encountered: " <> (T.pack $ Prelude.show invalid)


data Protocol = MockedBFT
  deriving (Eq, Show)

-- Node can be run in two modes.
data ViewMode = LiveView    -- Live mode with TUI
              | SimpleView  -- Simple mode, just output text.
              deriving (Eq, Show)

parseNodeConfiguration :: FilePath -> IO NodeConfiguration
parseNodeConfiguration fp = decodeFileThrow fp

-- | Core configuration.
-- For now, we only store the path to the genesis file(s) and their hash.
-- The rest is in the hands of the modules/features that need to use it.
-- The info flow is:
-- __genesis config ---> genesis file__
-- And separately:
-- __genesis file ---> runtime config ---> running node__
-- __static config ---> ...__
data Core = Core
    { coGenesisFile                 :: !FilePath
    -- ^ Genesis source file JSON.
    , coGenesisHash                 :: !Text
    -- ^ Genesis previous block hash.
    , coNodeId                      :: !(Maybe Int)
    -- ^ Core node ID, the number of the node.
    , coNumCoreNodes                :: !(Maybe Int)
    -- ^ The number of the core nodes.
    , coStaticKeySigningKeyFile     :: !(Maybe FilePath)
    -- ^ Static key signing file.
    , coStaticKeyDlgCertFile        :: !(Maybe FilePath)
    -- ^ Static key delegation certificate.
    , coRequiresNetworkMagic        :: !RequiresNetworkMagic
    -- ^ Do we require the network byte indicator for mainnet, testnet or staging?
    , coPBftSigThd                  :: !(Maybe Double)
    -- ^ PBFT signature threshold system parameters

    } deriving (Eq, Show)

data NodeCLI = NodeCLI
    { mscFp :: !MiscellaneousFilepaths
    , genesisHash :: !Text
    , nodeAddr :: !NodeAddress
    , configFp :: !ConfigYamlFilePath
    , traceOpts :: !TraceOptions
    , validateDB :: !Bool
    } deriving Show

data MiscellaneousFilepaths = MiscellaneousFilepaths
  { topFile :: !TopologyFile
  , dBFile :: !DbFile
  , genesisFile :: !GenesisFile
  , delegCertFile :: !(Maybe DelegationCertFile)
  , signKeyFile :: !(Maybe SigningKeyFile)
  , socketFile :: !SocketFile
  } deriving Show

newtype TopologyFile = TopologyFile
  { unTopology :: FilePath }
  deriving Show

newtype DbFile = DbFile
  { unDB :: FilePath }
  deriving Show

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath }
  deriving (Eq, Ord, Show, IsString)

newtype DelegationCertFile = DelegationCertFile
  { unDelegationCert :: FilePath }
  deriving Show

newtype SocketFile = SocketFile
  { unSocket :: FilePath }
  deriving Show

newtype SigningKeyFile = SigningKeyFile
  { unSigningKey ::  FilePath }
  deriving (Eq, Ord, Show, IsString)

-- TODO: migrate to Update.SoftwareVersion
data Update = Update
    { upApplicationName       :: !Update.ApplicationName
    -- ^ Update application name.
    , upApplicationVersion    :: !Update.NumSoftwareVersion
    -- application version.
    , upLastKnownBlockVersion :: !LastKnownBlockVersion
    -- ^ Update last known block version.
    } deriving (Eq, Show)

-- TODO: migrate to Update.ProtocolVersion
data LastKnownBlockVersion = LastKnownBlockVersion
    { lkbvMajor :: !Word16
    -- ^ Last known block version major.
    , lkbvMinor :: !Word16
    -- ^ Last known block version minor.
    , lkbvAlt   :: !Word8
    -- ^ Last known block version alternative.
    } deriving (Eq, Show)

nodeAddressToSockAddr :: NodeAddress -> SockAddr
nodeAddressToSockAddr (NodeAddress addr port) =
  case getAddress addr of
    Just (IP.IPv4 ipv4) -> SockAddrInet port $ IP.toHostAddress ipv4
    Just (IP.IPv6 ipv6) -> SockAddrInet6 port 0 (IP.toHostAddress6 ipv6) 0
    Nothing             -> SockAddrInet port 0 -- Could also be any IPv6 addr

nodeAddressInfo :: NodeAddress -> IO [AddrInfo]
nodeAddressInfo (NodeAddress hostAddr port) = do
  let hints = defaultHints {
                addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
              , addrSocketType = Stream
              }
  getAddrInfo (Just hints) (fmap show $ getAddress hostAddr) (Just $ show port)

-- | Detailed tracing options. Each option enables a tracer
--   which verbosity to the log output.
data TraceOptions = TraceOptions
  { traceVerbosity :: !TracingVerbosity
  , traceChainDB :: !Bool
    -- ^ By default we use 'readableChainDB' tracer, if on this it will use
    -- more verbose tracer

    -- Consensus Tracers --
  , traceChainSyncClient :: !Bool
  , traceChainSyncHeaderServer :: !Bool
  , traceChainSyncBlockServer :: !Bool
  , traceBlockFetchDecisions :: !Bool
  , traceBlockFetchClient :: !Bool
  , traceBlockFetchServer :: !Bool
  , traceTxInbound :: !Bool
  , traceTxOutbound :: !Bool
  , traceLocalTxSubmissionServer :: !Bool
  , traceMempool :: !Bool
  , traceForge :: !Bool
    -----------------------

    -- Protocol Tracers --
  , traceChainSyncProtocol :: !Bool
    -- There's two variants of the block fetch tracer and for now
    -- at least we'll set them both together from the same flags.
  , traceBlockFetchProtocol :: !Bool
  , traceBlockFetchProtocolSerialised :: !Bool
  , traceTxSubmissionProtocol :: !Bool
  , traceLocalChainSyncProtocol :: !Bool
  , traceLocalTxSubmissionProtocol :: !Bool
  , traceIpSubscription :: !Bool
    -----------------------

  , traceDnsSubscription :: !Bool
  , traceDnsResolver :: !Bool
  , traceErrorPolicy :: !Bool
  , traceMux :: !Bool
  , traceLedgerState :: !Bool
  , traceBlockchainRpc :: !Bool
  } deriving (Eq, Show)

newtype ConfigYamlFilePath = ConfigYamlFilePath
  { unConfigPath :: FilePath }
  deriving Show
