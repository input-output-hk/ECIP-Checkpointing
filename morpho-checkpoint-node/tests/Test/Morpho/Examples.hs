{-# LANGUAGE OverloadedStrings #-}

module Test.Morpho.Examples where

import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Crypto.ProtocolMagic
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence.Strict as Seq
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Morpho.Common.Bytes
import Morpho.Common.Conversions
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.State
import Morpho.Ledger.Tx
import Morpho.Ledger.Update
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Network.Block hiding (castHash)
import Ouroboros.Network.Point
import Prelude

type TestBlock = MorphoBlock MorphoMockHash ConsensusMockCrypto

type TestStdHeader = MorphoStdHeader MorphoMockHash ConsensusMockCrypto

exampleBlock :: TestBlock
exampleBlock = MorphoBlock exampleHeader exampleBody

exampleLedgerState :: LedgerState TestBlock
exampleLedgerState = MorphoLedgerState exampleMorphoState

exampleMorphoState :: MorphoState TestBlock
exampleMorphoState =
  MorphoState
    { lastCheckpoint = exampleCheckpoint,
      checkpointAt = examplePoint,
      currentVotes = M.fromList [(examplePublicKey, exampleVote)],
      morphoTip = examplePoint
    }

exampleStdHeader :: TestStdHeader
exampleStdHeader =
  MorphoStdHeader
    { morphoPrev = GenesisHash,
      morphoSlotNo = SlotNo 5,
      morphoBlockNo = BlockNo 3,
      morphoBodyHash = castHash $ hash (1 :: Int),
      morphoBlockSize = 100
    }

exampleChainHash :: ChainHash TestBlock
exampleChainHash = BlockHash $ castHash $ hash (0 :: Int)

exampleBody :: MorphoBody
exampleBody = MorphoBody [exampleMorphoBlockTx]

exampleMorphoBlockTx :: MorphoBlockTx
exampleMorphoBlockTx = MorphoBlockTx tx txId
  where
    MorphoGenTx tx txId = exampleGenTx

exampleTx :: Tx
exampleTx = Tx exampleVote

exampleGenTx :: GenTx TestBlock
exampleGenTx = mkMorphoGenTx exampleTx

exampleHeader :: Header TestBlock
exampleHeader =
  MorphoHeader
    { morphoHeaderHash = castHash $ hash (10 :: Int),
      morphoHeaderStd = exampleStdHeader,
      morphoBftFields =
        BftFields $ SignedDSIGN $ SigMockDSIGN (castHash $ hash (10 :: Int)) 20
    }

examplePoint :: Point TestBlock
examplePoint =
  Point
    { getPoint = block 4 (castHash $ hash True)
    }

examplePublicKey :: PublicKey
examplePublicKey = pKey $ keyPairFromPrivate examplePrivateKey

exampleVote :: Vote
exampleVote =
  Vote
    { votedPowBlock = examplePowBlockRef,
      voteSignature = exampleSignature
    }

examplePrivateKey :: PrivateKey
examplePrivateKey = fromMaybe (error "privateKey") $ importPrivateKey $ bytesFromHex $ T.pack $ replicate 64 'a'

exampleSignature :: Signature
exampleSignature =
  Signature
    { sign_r = Bytes $ B.pack [0 .. 15],
      sign_s = Bytes $ B.pack [16 .. 32],
      sign_v = 5
    }

exampleCheckpoint :: Checkpoint
exampleCheckpoint =
  Checkpoint
    { checkpointedBlock = examplePowBlockRef,
      chkpSignatures = [exampleSignature]
    }

examplePowBlockRef :: PowBlockRef
examplePowBlockRef =
  PowBlockRef
    { powBlockNo = PowBlockNo 10,
      powBlockHash = PowBlockHash $ Bytes $ B.pack [0 .. 15]
    }

exampleBftConfig :: ConsensusConfig (Bft ConsensusMockCrypto)
exampleBftConfig =
  BftConfig
    { bftParams = -- not used to forge blocks
        BftParams
          { bftSecurityParam = SecurityParam 4,
            bftNumNodes = NumCoreNodes 5
          },
      bftSignKey = SignKeyMockDSIGN 1,
      bftVerKeys = mempty -- not used to forge blocks
    }

exampleHeaderHash :: HeaderHash TestBlock
exampleHeaderHash = castHash $ hash (10 :: Int)

exampleExtLedgerState :: ExtLedgerState TestBlock
exampleExtLedgerState =
  ExtLedgerState
    { ledgerState = exampleLedgerState,
      headerState = exampleHeaderState
    }

exampleHeaderState :: HeaderState TestBlock
exampleHeaderState =
  HeaderState
    { headerStateConsensus = (),
      headerStateTips = Seq.singleton exampleAnnTip,
      headerStateAnchor = Origin
    }

exampleAnnTip :: AnnTip TestBlock
exampleAnnTip =
  AnnTip
    { annTipSlotNo = 0,
      annTipBlockNo = 0,
      annTipInfo = exampleHeaderHash
    }

exampleApplyTxErr :: MorphoError TestBlock
exampleApplyTxErr = MorphoInvalidSignature exampleVote

exampleNodeConfig :: NodeConfiguration
exampleNodeConfig =
  NodeConfiguration
    { ncProtocol = MockedBFT,
      ncNodeId = CoreId $ CoreNodeId 0,
      ncNumCoreNodes = 1,
      ncReqNetworkMagic = RequiresMagic,
      ncNetworkMagic = 12345,
      ncSystemStart = Just $ SystemStart $ posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer),
      ncSecurityParameter = 3,
      ncLoggingSwitch = True,
      ncTraceOpts = exampleTraceOptions,
      ncLogMetrics = True,
      ncViewMode = SimpleView,
      ncUpdate = Update $ LastKnownBlockVersion 0 2 0,
      ncTimeslotLength = mkSlotLength 5,
      ncSnapshotsOnDisk = 60,
      ncSnapshotInterval = 60,
      ncPoWBlockFetchInterval = Just 5000000,
      ncPoWNodeRpcUrl = "http://127.0.0.1:8546",
      ncPrometheusPort = 13788,
      ncCheckpointInterval = 4,
      ncRequiredMajority = 1,
      ncFedPubKeys = [publicKey],
      ncNodePrivKeyFile = "abc"
    }
  where
    hex = fromMaybe (error "publicKey") $ normalizeHex "ec33a3689573db2f4db4586bb7089cda045116a21cce20c9a6fe7ccadcf9fb336075b3644ac9f0a20e6d45a9e99db477cc420d050969f2d8bfb7408b2169b167"
    publicKey = fromMaybe (error "publicKey") $ importPublicKey $ bytesFromHex hex

exampleTraceOptions :: TraceOptions
exampleTraceOptions = TraceOptions {
    traceVerbosity = NormalVerbosity
  , traceChainDB = True
  , traceChainSyncClient = True
  , traceChainSyncHeaderServer = True
  , traceChainSyncBlockServer = True
  , traceBlockFetchDecisions = True
  , traceBlockFetchServer = True
  , traceBlockFetchClient = True
  , traceTxInbound = True
  , traceTxOutbound = True
  , traceLocalTxSubmissionServer = True
  , traceMempool = True
  , traceForge = True
  , traceChainSyncProtocol = True
  , traceBlockFetchProtocol = True
  , traceBlockFetchProtocolSerialised = False
  , traceTxSubmissionProtocol = True
  , traceLocalChainSyncProtocol = True
  , traceLocalTxSubmissionProtocol = True
  , traceLocalStateQueryProtocol = True
  , traceIpSubscription = False
  , traceDnsSubscription = False
  , traceDnsResolver = False
  , traceErrorPolicy = False
  , traceMux = False
  , traceHandshake = True
  , traceLedgerState = True
  , tracePoWNodeRpc = True
  , traceTimeTravelError = True
  }


exampleTopology :: NetworkTopology
exampleTopology = NetworkTopology [t1]
  where
    t1 =
      NodeSetup
        { nodeId = 0,
          nodeAddress = (NodeAddress (NodeHostAddress (Just "18.130.216.242")) 3000),
          producers = [RemoteAddress "3.10.235.25" 3000 1]
        }
