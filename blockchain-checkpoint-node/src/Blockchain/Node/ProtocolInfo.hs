module Blockchain.Node.ProtocolInfo (
    ProtocolInfoBft
  , protocolInfoBft
  ) where

import Cardano.Prelude
import qualified Data.Map as Map
import Control.Monad (fail)

import           Cardano.Crypto.DSIGN
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (NodeId(..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT

import           Blockchain.Config.Types
import           Blockchain.Common.Conversions
import           Blockchain.Ledger.Block
import           Blockchain.Ledger.BFT
import           Blockchain.Ledger.State
import           Blockchain.Crypto.ECDSASignature (importPrivateKey, keyPairFromPrivate)

type ProtocolInfoBft = ProtocolInfo (MorphoBftBlock MorphoMockCrypto MorphoMockCrypto)


protocolInfoBft :: (MonadIO m) =>
                   NodeConfiguration
                -> SecurityParam
                -> m ProtocolInfoBft
protocolInfoBft nc securityParam = do
  privKeyStr <- liftIO . readFile $ ncNodePrivKeyFile nc
  privKey <- do
    case importPrivateKey $ bytesFromHex privKeyStr of
      Nothing -> fail $ "Invalid private key in: " <> show (ncNodePrivKeyFile nc)
      Just pk -> return pk

  let ledgerConfig = MorphoLedgerConfig {
        checkpointingInterval = ncCheckpointInterval nc
      , requiredMajority      = ncRequiredMajority nc
      , fedPubKeys            = ncFedPubKeys nc
      , nodeKeyPair           = keyPairFromPrivate privKey
  }
  pure ProtocolInfo {
      pInfoConfig = BftNodeConfig {
          bftParams   = BftParams {
                            bftNumNodes      = fromIntegral $ ncNumCoreNodes nc
                          , bftSecurityParam = securityParam
                          , bftSlotLengths   = singletonSlotLengths $ ncTimeslotLength nc
                          }
        , bftNodeId   = ncNodeId nc
        , bftSignKey  = SignKeyMockDSIGN nid
        , bftVerKeys  = Map.fromList [
              (CoreId n, VerKeyMockDSIGN n)
            | n <- [0 .. (ncNumCoreNodes nc) - 1]
            ]
        }
    , pInfoInitLedger = ExtLedgerState (genesisMorphoLedgerState ledgerConfig) ()
    , pInfoInitState  = ()
    }
  where
    CoreId nid = ncNodeId nc
    -- Not sure if we need this:
    -- kinda unsafe but don't immediately see how to ensure the node is a core one. Besides, we won't have any relay...
    -- toCoreId :: NodeId -> CoreNodeId
    -- toCoreId (CoreId i) = CoreNodeId i
    -- toCoreId (RelayId _) = error "Blockchain-checkpointing-nodes cannot be relays."
