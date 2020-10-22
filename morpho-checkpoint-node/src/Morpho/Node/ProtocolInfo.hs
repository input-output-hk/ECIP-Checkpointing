module Morpho.Node.ProtocolInfo
  ( protocolInfoMorpho,
  )
where

import Cardano.Crypto.DSIGN
import Cardano.Prelude
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad (fail)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as Seq
import Morpho.Common.Conversions
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature (importPrivateKey, keyPairFromPrivate)
import Morpho.Ledger.Block
import Morpho.Ledger.Forge ()
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block.Forge
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import Ouroboros.Consensus.Protocol.BFT

protocolInfoMorpho ::
  MonadIO m =>
  NodeConfiguration ->
  SecurityParam ->
  m (ProtocolInfo m (MorphoBlock MorphoMockHash ConsensusMockCrypto))
protocolInfoMorpho nc securityParam = do
  privKeyStr <- liftIO . readFile $ ncNodePrivKeyFile nc
  privKey <- do
    case importPrivateKey $ bytesFromHex privKeyStr of
      Nothing -> fail $ "Invalid private key in: " <> show (ncNodePrivKeyFile nc)
      Just pk -> return pk
  let ledgerConfig =
        MorphoLedgerConfig
          { checkpointingInterval = ncCheckpointInterval nc,
            requiredMajority = ncRequiredMajority nc,
            fedPubKeys = ncFedPubKeys nc,
            slotLength = ncTimeslotLength nc,
            nodeKeyPair = keyPairFromPrivate privKey
          }
  pure
    ProtocolInfo
      { pInfoConfig =
          TopLevelConfig
            { topLevelConfigProtocol =
                FullProtocolConfig
                  { protocolConfigConsensus = bftConfig,
                    protocolConfigIndep = ()
                  },
              topLevelConfigBlock =
                FullBlockConfig
                  { blockConfigLedger = ledgerConfig,
                    blockConfigBlock = MorphoBlockConfig securityParam,
                    blockConfigCodec = MorphoCodecConfig securityParam
                  }
            },
        pInfoInitLedger =
          ExtLedgerState
            { ledgerState = genesisMorphoLedgerState,
              headerState = HeaderState () Seq.Empty Origin
            },
        pInfoLeaderCreds = Just (toCoreId (ncNodeId nc), defaultMaintainForgeState)
      }
  where
    bftConfig =
      BftConfig
        { bftParams =
            BftParams
              { bftSecurityParam = securityParam,
                bftNumNodes = NumCoreNodes $ ncNumCoreNodes nc
              },
          bftSignKey = SignKeyMockDSIGN nId,
          bftVerKeys =
            Map.fromList
              [ (CoreId n, verKey n)
                | n <- enumCoreNodes (NumCoreNodes $ ncNumCoreNodes nc)
              ]
        }
    coreId :: CoreNodeId
    coreId = toCoreId (ncNodeId nc)
    CoreNodeId nId = coreId
    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n
    -- We won't have any relay
    toCoreId :: NodeId -> CoreNodeId
    toCoreId (CoreId cni) = cni
    toCoreId (RelayId _) = panic "OBFT-checkpointing-nodes cannot be relays."
