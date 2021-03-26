module Morpho.Node.Env where

import Cardano.BM.Data.Transformers
import Cardano.BM.Trace
import Cardano.Crypto.DSIGN
import Cardano.Prelude
import Cardano.Shell.Lib
import Control.Monad.Fail
import Control.Tracer
import qualified Data.List as List
import qualified Data.Text as T
import Morpho.Config.Logging
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.Update
import Morpho.Tracing.Tracers
import Morpho.Tracing.Types
import Network.HostName
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Network.Magic
import Ouroboros.Network.NodeToClient
import Ouroboros.Network.NodeToNode (RemoteConnectionId)
import System.Directory

-- | Turns the user configuration (which can be provided by the CLI, config
-- files or from defaults) into an application environment. This function takes
-- care of initializing/checking values provided by the config
withEnv ::
  ( MorphoStateDefaultConstraints h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  NodeConfiguration ->
  (Env h c -> IO ()) ->
  IO ()
withEnv nc action = do
  privKey <- initPrivateKey nc

  withTracing nc $ \tracers -> do
    prods <- initProducers tracers nc

    databaseDir <- initDatabaseDir nc

    action
      Env
        { eNodeId = ncNodeId nc,
          eNumCoreNodes = NumCoreNodes $ ncNumCoreNodes nc,
          eCheckpointingInterval = ncCheckpointInterval nc,
          eRequiredMajority = ncRequiredMajority nc,
          eFedPubKeys = ncFedPubKeys nc,
          eTimeslotLength = ncTimeslotLength nc,
          eNetworkMagic = NetworkMagic $ ncNetworkMagic nc,
          eSecurityParameter = SecurityParam $ ncSecurityParameter nc,
          eSystemStart = ncSystemStart nc,
          ePrivateKey = privKey,
          eTracers = tracers,
          ePrometheusPort = ncPrometheusPort nc,
          eSnapshotsOnDisk = fromIntegral $ ncSnapshotsOnDisk nc,
          eSnapshotInterval = ncSnapshotInterval nc,
          ePoWBlockFetchInterval = ncPoWBlockFetchInterval nc,
          ePoWNodeRpcUrl = ncPoWNodeRpcUrl nc,
          eStableLedgerDepth = ncStableLedgerDepth nc,
          eProducers = prods,
          eDatabaseDir = databaseDir,
          eSocketFile = unSocket $ ncSocketFile nc,
          eNodeAddress = NodeAddress (ncNodeHost nc) (ncNodePort nc),
          eValidateDb = ncValidateDb nc
        }

-- Read and import private key
initPrivateKey :: NodeConfiguration -> IO PrivateKey
initPrivateKey nc = do
  mprivKey <- liftIO . readPrivateKey $ ncNodePrivKeyFile nc
  case mprivKey of
    Left err -> fail $ "Failed to import private key from " <> show (ncNodePrivKeyFile nc) <> ": " <> show err
    Right pk -> return pk

-- Set up tracers from logging config
withTracing ::
  ( MorphoStateDefaultConstraints h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  NodeConfiguration ->
  (Tracers RemoteConnectionId LocalConnectionId h c -> IO ()) ->
  IO ()
withTracing nc action = do
  host <- T.take 8 . fst . T.breakOn "." . T.pack <$> getHostName
  (loggingLayer, loggingFeats) <- loggingFeatures (ncLogging nc) (ncLoggingSwitch nc)
  let basicTrace = setHostname host $ appendName "node" (llBasicTrace loggingLayer)
  tracers <- mkTracers (ncTraceOpts nc) basicTrace
  runCardanoApplicationWithFeatures loggingFeats $
    CardanoApplication $ action tracers

-- Read and import topology file
initProducers :: Tracers RemoteConnectionId LocalConnectionId h c -> NodeConfiguration -> IO [RemoteAddress]
initProducers tracers nc = do
  let file = unTopology $ ncTopologyFile nc
      nid = ncNodeId nc
  Right (NetworkTopology topology) <- readTopologyFile file

  case List.lookup nid $
    map (\ns -> (CoreNodeId $ nodeId ns, producers ns)) topology of
    Just ps -> do
      traceWith (morphoInitTracer tracers) $ ProducerList nid ps
      return ps
    Nothing -> do
      traceWith (morphoInitTracer tracers) $ NotFoundInTopology nid
      exitFailure

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
    eProducers :: [RemoteAddress],
    eDatabaseDir :: FilePath,
    eSocketFile :: FilePath,
    eNodeAddress :: NodeAddress,
    eValidateDb :: Bool
  }
