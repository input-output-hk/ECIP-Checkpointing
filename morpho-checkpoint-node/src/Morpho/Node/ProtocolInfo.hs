module Morpho.Node.ProtocolInfo
  ( protocolInfoMorpho,
  )
where

import Cardano.Crypto.DSIGN
import Cardano.Prelude
import qualified Data.Map as Map
import Morpho.Crypto.ECDSASignature (keyPairFromPrivate)
import Morpho.Ledger.Block
import Morpho.Ledger.Forge (morphoBlockForging)
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Morpho.Node.Env
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import Ouroboros.Consensus.Protocol.BFT

protocolInfoMorpho :: Monad m => Env rpce MorphoMockHash ConsensusMockCrypto -> ProtocolInfo m (MorphoBlock MorphoMockHash ConsensusMockCrypto)
protocolInfoMorpho env =
  ProtocolInfo
    { pInfoConfig =
        TopLevelConfig
          { topLevelConfigProtocol = bftConfig,
            topLevelConfigLedger = ledgerConfig,
            topLevelConfigBlock = blockConfig,
            topLevelConfigCodec = MorphoCodecConfig (),
            topLevelConfigStorage = MorphoStorageConfig secParam
          },
      pInfoInitLedger =
        ExtLedgerState
          { ledgerState = genesisMorphoLedgerState,
            headerState = HeaderState Origin ()
          },
      pInfoBlockForging = return [morphoBlockForging (eNodeId env)]
    }
  where
    ledgerConfig =
      MorphoLedgerConfig
        { checkpointingInterval = eCheckpointingInterval env,
          securityParam = secParam,
          requiredMajority = eRequiredMajority env,
          fedPubKeys = eFedPubKeys env,
          slotLength = eTimeslotLength env,
          nodeKeyPair = keyPairFromPrivate (ePrivateKey env)
        }
    blockConfig =
      MorphoBlockConfig
        { systemStart = eSystemStart env,
          networkMagic = eNetworkMagic env
        }
    secParam = eSecurityParameter env
    bftConfig =
      BftConfig
        { bftParams =
            BftParams
              { bftSecurityParam = secParam,
                bftNumNodes = eNumCoreNodes env
              },
          bftSignKey = SignKeyMockDSIGN nId,
          bftVerKeys =
            Map.fromList
              [ (CoreId n, verKey n)
                | n <- enumCoreNodes (eNumCoreNodes env)
              ]
        }
    CoreNodeId nId = eNodeId env
    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n
