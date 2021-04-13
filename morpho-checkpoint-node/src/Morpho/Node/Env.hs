module Morpho.Node.Env where

import Cardano.BM.Data.Transformers
import Cardano.Crypto.DSIGN
import Cardano.Prelude
import Cardano.Shell.Lib
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text as Text
import Morpho.Config.Logging
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.Update
import Morpho.RPC.Abstract
import Morpho.RPC.JsonRpc
import Morpho.Tracing.Tracers
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
  (Env JsonRpcEvent h c -> IO ()) ->
  IO ()
withEnv nc action = withTracing nc $ \tracers -> do
  privKey <- initPrivateKey nc

  prods <- initProducers nc

  rpcUpstream <- initRpcUpstream nc

  databaseDir <- initDatabaseDir nc

  action
    Env
      { eNodeId = ncNodeId nc,
        eNumCoreNodes = NumCoreNodes $ ncNumCoreNodes nc,
        eCheckpointingInterval = ncCheckpointInterval nc,
        eRequiredMajority = NumCoreNodes $ ncRequiredMajority nc,
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
        eRpcUpstream = rpcUpstream,
        eStableLedgerDepth = ncStableLedgerDepth nc,
        eProducers = prods,
        eDatabaseDir = databaseDir,
        eNodeAddress = NodeAddress (ncNodeHost nc) (ncNodePort nc),
        eValidateDb = ncValidateDb nc
      }

-- Read and import private key
initPrivateKey :: NodeConfiguration -> IO PrivateKey
initPrivateKey nc = do
  mprivKey <- liftIO . readPrivateKey $ ncNodePrivKeyFile nc
  case mprivKey of
    Left err -> do
      hPutStrLn stderr $ "Failed to import private key from " <> Text.pack (show (ncNodePrivKeyFile nc)) <> ": " <> show err
      exitFailure
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
  let basicTrace = setHostname host $ llBasicTrace loggingLayer
  tracers <- mkTracers nc basicTrace
  runCardanoApplicationWithFeatures loggingFeats $
    CardanoApplication $ action tracers

-- Read and import topology file
initProducers :: NodeConfiguration -> IO [RemoteAddress]
initProducers nc = do
  let file = unTopology $ ncTopologyFile nc
      nid = ncNodeId nc
  Right (NetworkTopology topology) <- readTopologyFile file

  case List.lookup nid $
    map (\ns -> (CoreNodeId $ nodeId ns, producers ns)) topology of
    Just ps -> return ps
    Nothing -> do
      hPutStrLn stderr $ "Node " <> Text.pack (show nid) <> " not found in topology file " <> Text.pack (show file)
      exitFailure

-- Make database path absolute
initDatabaseDir :: NodeConfiguration -> IO FilePath
initDatabaseDir nc = canonicalizePath =<< makeAbsolute (unDB $ ncDatabaseDir nc)

initRpcUpstream :: NodeConfiguration -> IO (RpcUpstream JsonRpcEvent IO)
initRpcUpstream nc = do
  mrpcUpstream <- runExceptT $ jsonRpcUpstream (ncPoWNodeRpcUrl nc)
  case mrpcUpstream of
    Left err -> do
      hPutStrLn stderr $ "Failed to initialize JSON RPC: " <> Text.pack (show err)
      exitFailure
    Right result -> return result

-- | The application environment, a read-only structure that configures how
-- morpho should run. This structure can abstract over interface details such
-- as logging, rpc, where configuration is read from, etc.
-- Essentially no initialization should be needed by morpho with such a value
data Env rpce h c = Env
  { eNodeId :: CoreNodeId,
    eNumCoreNodes :: NumCoreNodes,
    eCheckpointingInterval :: Int,
    eRequiredMajority :: NumCoreNodes,
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
    eRpcUpstream :: RpcUpstream rpce IO,
    eStableLedgerDepth :: Int,
    eProducers :: [RemoteAddress],
    eDatabaseDir :: FilePath,
    eNodeAddress :: NodeAddress,
    eValidateDb :: Bool
  }
