module Morpho.Node.Env where

import Cardano.BM.Data.Transformers
import Cardano.BM.Trace
import Cardano.Crypto.DSIGN
import Cardano.Prelude
import Control.Monad.Fail
import qualified Data.Text as T
import Data.Time
import Morpho.Config.Logging
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

getHostname :: IO Text
getHostname = do
  hn0 <- T.pack <$> getHostName
  return $ T.take 8 $ fst $ T.breakOn "." hn0

configurationToEnv ::
  ( MorphoStateDefaultConstraints h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  LoggingLayer ->
  NodeConfiguration Identity ->
  IO (Env h c)
configurationToEnv loggingLayer nc = do
  start <- maybe (SystemStart <$> getCurrentTime) pure (runIdentity $ ncSystemStart nc)

  mprivKey <- liftIO . readPrivateKey $ runIdentity $ ncNodePrivKeyFile nc
  privKey <- case mprivKey of
    Left err -> fail $ "Failed to import private key from " <> show (runIdentity $ ncNodePrivKeyFile nc) <> ": " <> show err
    Right pk -> return pk

  host <- getHostname
  let basicTrace =
        setHostname host $
          appendName "node" (llBasicTrace loggingLayer)

  tracers <- mkTracers (runIdentity $ ncTraceOpts nc) basicTrace

  return
    Env
      { eNodeId = runIdentity $ ncNodeId nc,
        eNumCoreNodes = NumCoreNodes $ runIdentity $ ncNumCoreNodes nc,
        eCheckpointingInterval = runIdentity $ ncCheckpointInterval nc,
        eRequiredMajority = runIdentity $ ncRequiredMajority nc,
        eFedPubKeys = runIdentity $ ncFedPubKeys nc,
        eTimeslotLength = runIdentity $ ncTimeslotLength nc,
        eNetworkMagic = NetworkMagic $ runIdentity $ ncNetworkMagic nc,
        eSecurityParameter = SecurityParam $ runIdentity $ ncSecurityParameter nc,
        eSystemStart = start,
        ePrivateKey = privKey,
        eTracers = tracers,
        ePrometheusPort = runIdentity $ ncPrometheusPort nc,
        eSnapshotsOnDisk = fromIntegral $ ncSnapshotsOnDisk nc,
        eSnapshotInterval = runIdentity $ ncSnapshotInterval nc,
        ePoWBlockFetchInterval = fromMaybe (1000 * 1000) $ runIdentity $ ncPoWBlockFetchInterval nc,
        ePoWNodeRpcUrl = runIdentity $ ncPoWNodeRpcUrl nc,
        eStableLedgerDepth = runIdentity $ ncStableLedgerDepth nc
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
    eStableLedgerDepth :: Int
  }
