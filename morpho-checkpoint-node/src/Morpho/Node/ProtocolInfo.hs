module Morpho.Node.ProtocolInfo
  ( protocolInfoMorpho,
  )
where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.ProtocolMagic
import Cardano.Prelude
import Cardano.Slotting.Slot (WithOrigin (..))
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as Seq
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature (PrivateKey, keyPairFromPrivate)
import Morpho.Ledger.Block
import Morpho.Ledger.Forge ()
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block.Forge
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Network.Magic

protocolInfoMorpho :: Monad m => NodeConfiguration -> PrivateKey -> SystemStart -> ProtocolInfo m (MorphoBlock MorphoMockHash ConsensusMockCrypto)
protocolInfoMorpho nc privKey start =
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
                  blockConfigBlock = blockConfig,
                  blockConfigCodec = MorphoCodecConfig ()
                }
          },
      pInfoInitLedger =
        ExtLedgerState
          { ledgerState = genesisMorphoLedgerState,
            headerState = HeaderState () Seq.Empty Origin
          },
      pInfoLeaderCreds = Just (ncNodeId nc, defaultMaintainForgeState)
    }
  where
    ledgerConfig =
      MorphoLedgerConfig
        { checkpointingInterval = ncCheckpointInterval nc,
          securityParam = secParam,
          requiredMajority = ncRequiredMajority nc,
          fedPubKeys = ncFedPubKeys nc,
          slotLength = ncTimeslotLength nc,
          nodeKeyPair = keyPairFromPrivate privKey
        }
    blockConfig =
      MorphoBlockConfig
        { systemStart = start,
          networkMagic = NetworkMagic (ncNetworkMagic nc),
          protocolMagicId = ProtocolMagicId (ncNetworkMagic nc)
        }
    secParam = SecurityParam $ ncSecurityParameter nc
    bftConfig =
      BftConfig
        { bftParams =
            BftParams
              { bftSecurityParam = secParam,
                bftNumNodes = NumCoreNodes $ ncNumCoreNodes nc
              },
          bftSignKey = SignKeyMockDSIGN nId,
          bftVerKeys =
            Map.fromList
              [ (CoreId n, verKey n)
                | n <- enumCoreNodes (NumCoreNodes $ ncNumCoreNodes nc)
              ]
        }
    CoreNodeId nId = ncNodeId nc
    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n
