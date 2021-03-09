module Morpho.Node.Env where

import Cardano.BM.Data.Transformers
import Cardano.BM.Trace
import Cardano.Crypto.DSIGN
import Cardano.Prelude
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

getHostname :: IO Text
getHostname = do
  hn0 <- T.pack <$> getHostName
  return $ T.take 8 $ fst $ T.breakOn "." hn0

configurationToEnv ::
  ( MorphoStateDefaultConstraints h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  LoggingLayer ->
  NodeConfiguration ->
  IO (Env h c)
configurationToEnv loggingLayer nc = do
  start <- maybe (SystemStart <$> getCurrentTime) pure (ncSystemStart nc)

  mprivKey <- liftIO . readPrivateKey $ ncNodePrivKeyFile nc
  privKey <- case mprivKey of
    Left err -> fail $ "Failed to import private key from " <> show (ncNodePrivKeyFile nc) <> ": " <> show err
    Right pk -> return pk

  host <- getHostname
  let basicTrace =
        setHostname host $
          appendName "node" (llBasicTrace loggingLayer)

  tracers <- mkTracers (ncTraceOpts nc) basicTrace

  topology <-
    either error id <$> readTopologyFile (unTopology $ ncTopologyFile nc)

  databaseDir <- canonicalizePath =<< makeAbsolute (unDB $ ncDatabaseDir nc)

  return
    Env
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
        eSocketFile = unSocket $ ncSocketFile nc,
        eNodeAddress = NodeAddress (ncNodeHost nc) (ncNodePort nc),
        eValidateDb = ncValidateDb nc
      }

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
    eSocketFile :: FilePath,
    eNodeAddress :: NodeAddress,
    eValidateDb :: Bool
  }
