module Morpho.Node.Env where

import Cardano.BM.Data.Transformers
import Cardano.BM.Trace
import Cardano.Crypto.DSIGN
import Cardano.Prelude
import Cardano.Shell.Types
import Control.Monad.Fail
import qualified Data.Text as T
import Data.Time
import Morpho.Config.Logging
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.Update
import Morpho.Tracing.Tracers
import Network.HostName
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Network.Magic
import Ouroboros.Network.NodeToClient
import Ouroboros.Network.NodeToNode
import System.Directory
import Prelude (error, id)

-- | Turns the user configuration (which can be provided by the CLI, config
-- files or from defaults) into an application environment. This function takes
-- care of initializing/checking values provided by the config
configurationToEnv ::
  ( MorphoStateDefaultConstraints h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  NodeConfiguration ->
  IO (Env h c, [CardanoFeature])
configurationToEnv nc = do
  start <- initSystemStart nc

  privKey <- initPrivateKey nc

  (tracers, loggingFeats) <- initTracers nc

  topology <- initTopology nc

  databaseDir <- initDatabaseDir nc

  return
    ( Env
        { eNodeId = ncNodeId nc,
          eNumCoreNodes = NumCoreNodes $ ncNumCoreNodes nc,
          eCheckpointingInterval = ncCheckpointInterval nc,
          eRequiredMajority = ncRequiredMajority nc,
          eFedPubKeys = ncFedPubKeys nc,
          eTimeslotLength = ncTimeslotLength nc,
          eNetworkMagic = NetworkMagic $ ncNetworkMagic nc,
          eSecurityParameter = SecurityParam $ ncSecurityParameter nc,
          eSystemStart = start,
          ePrivateKey = privKey,
          eTracers = tracers,
          ePrometheusPort = ncPrometheusPort nc,
          eSnapshotsOnDisk = fromIntegral $ ncSnapshotsOnDisk nc,
          eSnapshotInterval = ncSnapshotInterval nc,
          ePoWBlockFetchInterval = ncPoWBlockFetchInterval nc,
          ePoWNodeRpcUrl = ncPoWNodeRpcUrl nc,
          eStableLedgerDepth = ncStableLedgerDepth nc,
          eTopology = topology,
          eDatabaseDir = databaseDir,
          eNodeAddress = NodeAddress (ncNodeHost nc) (ncNodePort nc),
          eValidateDb = ncValidateDb nc
        },
      loggingFeats
    )

-- Set current time as system start if not provided
initSystemStart :: NodeConfiguration -> IO SystemStart
initSystemStart nc = maybe (SystemStart <$> getCurrentTime) pure (ncSystemStart nc)

-- Read and import private key
initPrivateKey :: NodeConfiguration -> IO PrivateKey
initPrivateKey nc = do
  mprivKey <- liftIO . readPrivateKey $ ncNodePrivKeyFile nc
  case mprivKey of
    Left err -> fail $ "Failed to import private key from " <> show (ncNodePrivKeyFile nc) <> ": " <> show err
    Right pk -> return pk

-- Set up tracers from logging config
initTracers ::
  ( MorphoStateDefaultConstraints h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  NodeConfiguration ->
  IO (Tracers RemoteConnectionId LocalConnectionId h c, [CardanoFeature])
initTracers nc = do
  host <- T.take 8 . fst . T.breakOn "." . T.pack <$> getHostName
  (loggingLayer, loggingFeats) <- loggingFeatures (ncLogging nc) (ncLoggingSwitch nc)
  let basicTrace = setHostname host $ appendName "node" (llBasicTrace loggingLayer)
  tracers <- mkTracers (ncTraceOpts nc) basicTrace
  return (tracers, loggingFeats)

-- Read and import topology file
initTopology :: NodeConfiguration -> IO NetworkTopology
initTopology nc = either error id <$> readTopologyFile (unTopology $ ncTopologyFile nc)

-- Make database path absolute
initDatabaseDir :: NodeConfiguration -> IO FilePath
initDatabaseDir nc = canonicalizePath =<< makeAbsolute (unDB $ ncDatabaseDir nc)

-- | The application environment, a read-only structure that configures how
-- morpho should run. This structure can abstract over interface details such
-- as logging, rpc, where configuration is read from, etc.
-- Essentially no initialization should be needed by morpho with such a value
data Env h c = Env
  { eNodeId :: CoreNodeId,
    eNumCoreNodes :: NumCoreNodes,
    eCheckpointingInterval :: Int,
    eRequiredMajority :: Int,
    eFedPubKeys :: [PublicKey],
    eTimeslotLength :: SlotLength,
    eNetworkMagic :: NetworkMagic,
    eSecurityParameter :: SecurityParam,
    eSystemStart :: SystemStart,
    ePrivateKey :: PrivateKey,
    eTracers :: Tracers RemoteConnectionId LocalConnectionId h c,
    ePrometheusPort :: Int,
    eSnapshotsOnDisk :: Word,
    eSnapshotInterval :: Word64,
    ePoWBlockFetchInterval :: Int,
    ePoWNodeRpcUrl :: Text,
    eStableLedgerDepth :: Int,
    eTopology :: NetworkTopology,
    eDatabaseDir :: FilePath,
    eNodeAddress :: NodeAddress,
    eValidateDb :: Bool
  }
