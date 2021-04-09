{-# LANGUAGE OverloadedStrings #-}

module Test.Morpho.Examples
  ( morphoExamples,
    exampleNodeConfig,
    exampleTopology,
    TestBlock,
    G.Examples (..),
  )
where

import Cardano.BM.Data.Backend
import Cardano.BM.Data.Configuration
import Cardano.BM.Data.Output
import Cardano.BM.Data.Severity
import Cardano.Binary
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Maybe
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
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Network.Block hiding (castHash)
import Ouroboros.Network.Point
import Test.Morpho.Common.Utils
import qualified Test.Util.Serialisation.Golden as G
import Prelude

type TestBlock = MorphoBlock MorphoMockHash ConsensusMockCrypto

type TestStdHeader = MorphoStdHeader MorphoMockHash ConsensusMockCrypto

morphoExamples :: G.Examples TestBlock
morphoExamples =
  mempty
    { G.exampleBlock = G.unlabelled exampleBlock,
      G.exampleHeader = G.unlabelled exampleHeader,
      G.exampleHeaderHash = G.unlabelled exampleHeaderHash,
      G.exampleGenTx = G.unlabelled exampleGenTx,
      G.exampleApplyTxErr = G.unlabelled exampleApplyTxErr,
      G.exampleAnnTip = G.unlabelled exampleAnnTip,
      G.exampleLedgerState = G.unlabelled exampleLedgerState,
      G.exampleExtLedgerState = G.unlabelled exampleExtLedgerState
    }

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
      morphoBodyHash = castHash $ hashWithSerialiser toCBOR (1 :: Int)
    }

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
    { morphoHeaderHash = castHash $ hashWithSerialiser toCBOR (10 :: Int),
      morphoHeaderStd = exampleStdHeader,
      morphoBftFields =
        BftFields $ SignedDSIGN $ SigMockDSIGN (castHash $ hashWithSerialiser toCBOR (10 :: Int)) 20,
      morphoBlockSize = 100
    }

examplePoint :: Point TestBlock
examplePoint =
  Point
    { getPoint = block 4 (castHash $ hashWithSerialiser toCBOR True)
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
examplePrivateKey = fromRight' $ importPrivateKey $ fromRight' $ bytesFromHex $ T.pack $ replicate 64 'a'

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

exampleHeaderHash :: HeaderHash TestBlock
exampleHeaderHash = castHash $ hashWithSerialiser toCBOR (10 :: Int)

exampleExtLedgerState :: ExtLedgerState TestBlock
exampleExtLedgerState =
  ExtLedgerState
    { ledgerState = exampleLedgerState,
      headerState = exampleHeaderState
    }

exampleHeaderState :: HeaderState TestBlock
exampleHeaderState =
  HeaderState
    { headerStateTip = Origin,
      headerStateChainDep = ()
    }

exampleAnnTip :: AnnTip TestBlock
exampleAnnTip =
  AnnTip
    { annTipSlotNo = 0,
      annTipBlockNo = 0,
      annTipInfo = exampleHeaderHash
    }

exampleApplyTxErr :: (Vote, MorphoTransactionError)
exampleApplyTxErr = (exampleVote, MorphoInvalidSignature)

exampleNodeConfig :: NodeConfiguration
exampleNodeConfig =
  NodeConfiguration
    { ncProtocol = MockedBFT,
      ncNodeId = CoreNodeId 3,
      ncNumCoreNodes = 5,
      ncNetworkMagic = 3254,
      ncSystemStart = SystemStart $ posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer),
      ncSecurityParameter = 123,
      ncStableLedgerDepth = 6,
      ncLoggingSwitch = False,
      ncTimeslotLength = mkSlotLength 2,
      ncSnapshotsOnDisk = 10,
      ncSnapshotInterval = 10,
      ncPoWBlockFetchInterval = 5000000,
      ncPoWNodeRpcUrl = "http://example.com:1234",
      ncPrometheusPort = 6543,
      ncCheckpointInterval = 6,
      ncRequiredMajority = 3,
      ncFedPubKeys = [publicKey],
      ncNodePrivKeyFile = "/path/to/private/key",
      ncTopologyFile = TopologyFile "/path/to/topo.json",
      ncDatabaseDir = DbFile "/path/to/db",
      ncNodeHost = NodeHostAddress (Just "127.0.0.1"),
      ncNodePort = 2345,
      ncValidateDb = True,
      ncVerbosity = 2,
      ncLogging =
        Representation
          { minSeverity = Info,
            rotation = Nothing,
            setupScribes =
              [ ScribeDefinition
                  { scKind = FileSK,
                    scFormat = ScText,
                    scName = "logs/staging.log",
                    scPrivacy = ScPublic,
                    scRotation = Nothing,
                    scMinSev = minBound,
                    scMaxSev = maxBound
                  },
                ScribeDefinition
                  { scKind = StdoutSK,
                    scFormat = ScText,
                    scName = "stdout",
                    scPrivacy = ScPublic,
                    -- v These are defaulted when parsing
                    scRotation = Nothing,
                    scMinSev = minBound,
                    scMaxSev = maxBound
                  }
              ],
            defaultScribes = [(FileSK, "logs/staging.log"), (StdoutSK, "stdout")],
            setupBackends = [KatipBK],
            defaultBackends = [KatipBK],
            hasEKG = Nothing,
            hasGraylog = Nothing,
            hasPrometheus = Nothing,
            hasGUI = Nothing,
            traceForwardTo = Nothing,
            forwardDelay = Nothing,
            traceAcceptAt = Nothing,
            options = mempty
          }
    }
  where
    hex = fromJust $ normalizeHex "ec33a3689573db2f4db4586bb7089cda045116a21cce20c9a6fe7ccadcf9fb336075b3644ac9f0a20e6d45a9e99db477cc420d050969f2d8bfb7408b2169b167"
    publicKey = fromRight' $ importPublicKey $ fromRight' $ bytesFromHex hex

exampleTopology :: NetworkTopology
exampleTopology = NetworkTopology [t1]
  where
    t1 =
      NodeSetup
        { nodeId = 0,
          producers = [RemoteAddress "3.10.235.25" (Just 3000) 1]
        }
