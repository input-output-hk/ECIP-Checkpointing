module Morpho.Node.Env where

import Cardano.Prelude
import Control.Monad.Fail
import Data.Time
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Network.Magic

configurationToEnv :: NodeConfiguration -> IO Env
configurationToEnv nc = do
  start <- maybe (SystemStart <$> getCurrentTime) pure (ncSystemStart nc)

  mprivKey <- liftIO . readPrivateKey $ ncNodePrivKeyFile nc
  privKey <- case mprivKey of
    Left err -> fail $ "Failed to import private key from " <> show (ncNodePrivKeyFile nc) <> ": " <> show err
    Right pk -> return pk

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
        eTraceOpts = ncTraceOpts nc,
        ePrometheusPort = ncPrometheusPort nc,
        eSnapshotsOnDisk = fromIntegral $ ncSnapshotsOnDisk nc,
        eSnapshotInterval = ncSnapshotInterval nc,
        ePoWBlockFetchInterval = fromMaybe (1000 * 1000) $ ncPoWBlockFetchInterval nc,
        ePoWNodeRpcUrl = ncPoWNodeRpcUrl nc,
        eStableLedgerDepth = ncStableLedgerDepth nc
      }

data Env = Env
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
    eTraceOpts :: TraceOptions,
    ePrometheusPort :: Int,
    eSnapshotsOnDisk :: Word,
    eSnapshotInterval :: Word64,
    ePoWBlockFetchInterval :: Int,
    ePoWNodeRpcUrl :: Text,
    eStableLedgerDepth :: Int
  }
