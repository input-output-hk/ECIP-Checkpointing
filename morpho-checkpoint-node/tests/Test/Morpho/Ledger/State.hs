{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Morpho.Ledger.State
  ( stateTests,
  )
where

import Cardano.Crypto (ProtocolMagicId (..))
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Prelude hiding ((.))
import Data.List ((!!))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Morpho.Common.Conversions
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.Forge
import Morpho.Ledger.PowTypes
import Morpho.Ledger.State
import Morpho.Ledger.Tx
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block (getHeader, headerPoint)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Network.Block hiding (castHash)
import Ouroboros.Network.Magic
import Ouroboros.Network.Point
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (show)

stateTests :: TestTree
stateTests =
  testGroup
    "State tests"
    [ testCase "Update checkpoint on receiving enough votes" assert_stateUpdateNewCheckpoint,
      testCase "Add votes without updating checkpoint when insufficient votes" assert_stateUpdateInsufficientVotes,
      testCase "Update single vote (change of mind)" assert_stateUpdateSingleVote,
      testCase "Fail to update state due to wrong checkpoint distance" assert_singleVoteWrongDistance,
      testCase "Fail to update state due to invalid vote signature" assert_singleVoteInvalidSignature,
      testCase "Fail to update state due to unknown vote signer" assert_singleVoteUnknownPublicKey,
      testCase "Fail to update state due to duplicate vote" assert_singleVoteDuplicateVote,
      testCase "Fail to update state due to invalid parent hash" assert_singleVoteInvalidHash
    ]

type TestBlock = MorphoBlock MorphoMockHash ConsensusMockCrypto

assert_stateUpdateNewCheckpoint ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_stateUpdateNewCheckpoint = case newStateResult of
  Left err -> assertFailure $ "failed to update state: " <> show err
  Right newState -> newState @?= expectedState
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.empty,
          morphoTip = makePoint 5
        }
    newVotes =
      mappend
        (makeVotes b2 (take 3 privateKeys))
        (makeVotes b3 [privateKeys !! 3])
    newBlock = makeBlock (morphoTip currentState) newVotes
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    newTip = headerPoint (getHeader newBlock)
    expectedState =
      currentState
        { -- TODO The permutation of signatures will change when we start sorting them.
          lastCheckpoint = makeCheckpoint b2 (fmap (privateKeys !!) [2, 0, 1]),
          checkpointAt = newTip,
          morphoTip = newTip
        }
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig) powBlockHash2
    b3 = makeBlockRef (interval testConfig) powBlockHash3

assert_stateUpdateInsufficientVotes ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_stateUpdateInsufficientVotes = case newStateResult of
  Left err -> assertFailure $ "failed to update state: " <> show err
  Right newState -> newState @?= expectedState
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.empty,
          morphoTip = makePoint 5
        }
    newVotes =
      mappend
        (makeVotes b2 (take 2 privateKeys))
        (makeVotes b3 (take 2 . drop 2 $ privateKeys))
    newBlock = makeBlock (morphoTip currentState) newVotes
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    newTip = headerPoint (getHeader newBlock)
    expectedState =
      currentState
        { currentVotes = M.fromList $ zip (take 4 publicKeys) newVotes,
          morphoTip = newTip
        }
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig) powBlockHash2
    b3 = makeBlockRef (interval testConfig) powBlockHash3

assert_stateUpdateSingleVote ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_stateUpdateSingleVote = case newStateResult of
  Left err -> assertFailure $ "failed to update state: " <> show err
  Right newState -> newState @?= expectedState
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.fromList $ zip (take 2 publicKeys) (makeVotes b2 (take 2 privateKeys)),
          morphoTip = makePoint 5
        }
    newVote = makeVote b3 (privateKeys !! 0)
    newBlock = makeBlock (morphoTip currentState) [newVote]
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    newTip = headerPoint (getHeader newBlock)
    expectedState =
      currentState
        { currentVotes = M.insert (publicKeys !! 0) (newVote) (currentVotes currentState),
          morphoTip = newTip
        }
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig) powBlockHash2
    b3 = makeBlockRef (interval testConfig) powBlockHash3

assert_singleVoteWrongDistance ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_singleVoteWrongDistance = case newStateResult of
  Left (MorphoWrongDistance _) -> pure ()
  Left err -> assertFailure $ "unexcpeted state update error: " <> show err
  Right _ -> assertFailure "excptected state update error"
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.empty,
          morphoTip = makePoint 5
        }
    newVote = makeVote b2 (privateKeys !! 0)
    newBlock = makeBlock (morphoTip currentState) [newVote]
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig + 1) powBlockHash2

assert_singleVoteInvalidSignature ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_singleVoteInvalidSignature = case newStateResult of
  Left (MorphoInvalidSignature _) -> pure ()
  Left err -> assertFailure $ "unexcpeted state update error: " <> show err
  Right _ -> assertFailure "excptected state update error"
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.empty,
          morphoTip = makePoint 5
        }
    invalidSig = Signature (bytesFromHex $ T.pack "cafebabe") (bytesFromHex $ T.pack "deadbeef") 42
    newVote = Vote b2 invalidSig
    newBlock = makeBlock (morphoTip currentState) [newVote]
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig) powBlockHash2

assert_singleVoteUnknownPublicKey ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_singleVoteUnknownPublicKey = case newStateResult of
  Left (MorphoUnknownPublicKey _) -> pure ()
  Left err -> assertFailure $ "unexcpeted state update error: " <> show err
  Right _ -> assertFailure "excptected state update error"
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.empty,
          morphoTip = makePoint 5
        }
    unknownPrivKey = fromJust $ importPrivateKey $ bytesFromHex $ T.pack "76c1e0e16c61b3ea3baa86eea46a7638d47bf6b8a554ac360d533d9b12c45a0a"
    newVote = makeVote b2 unknownPrivKey
    newBlock = makeBlock (morphoTip currentState) [newVote]
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig) powBlockHash2

assert_singleVoteDuplicateVote ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_singleVoteDuplicateVote = case newStateResult of
  Left (MorphoDuplicateVote _) -> pure ()
  Left err -> assertFailure $ "unexcpeted state update error: " <> show err
  Right _ -> assertFailure "excptected state update error"
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.insert (publicKeys !! 0) newVote M.empty,
          morphoTip = makePoint 5
        }
    newVote = makeVote b2 (privateKeys !! 0)
    newBlock = makeBlock (morphoTip currentState) [newVote]
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig) powBlockHash2

assert_singleVoteInvalidHash ::
  forall blk.
  (blk ~ TestBlock) =>
  Assertion
assert_singleVoteInvalidHash = case newStateResult of
  Left (MorphoInvalidHash _ _) -> pure ()
  Left err -> assertFailure $ "unexcpeted state update error: " <> show err
  Right _ -> assertFailure "excptected state update error"
  where
    currentState :: MorphoState blk
    currentState =
      MorphoState
        { lastCheckpoint = makeCheckpoint b1 privateKeys,
          checkpointAt = makePoint 0,
          currentVotes = M.empty,
          morphoTip = makePoint 5
        }
    newVote = makeVote b2 (privateKeys !! 0)
    newBlock = makeBlock (makePoint 6) [newVote]
    newStateResult = runExcept $ updateMorphoState testConfig newBlock currentState
    b1 = makeBlockRef 0 powBlockHash1
    b2 = makeBlockRef (interval testConfig) powBlockHash2

testConfig :: FullBlockConfig (LedgerState TestBlock) TestBlock
testConfig = FullBlockConfig
  {
    blockConfigLedger = MorphoLedgerConfig
      { checkpointingInterval = 4,
        securityParam = SecurityParam 4,
        requiredMajority = 3,
        fedPubKeys = publicKeys,
        nodeKeyPair = keyPairs !! 0,
        slotLength = mkSlotLength 2000
      },
    blockConfigBlock =
      MorphoBlockConfig
        {
          systemStart = SystemStart $ posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer),
          networkMagic = NetworkMagic 12345 ,
          protocolMagicId = ProtocolMagicId 12345
        },
    blockConfigCodec = MorphoCodecConfig ()
  }

interval :: FullBlockConfig (LedgerState TestBlock) TestBlock -> Int
interval = checkpointingInterval . blockConfigLedger

keyPairs :: [KeyPair]
keyPairs =
  hex2kp <$> hexes
  where
    hex2kp (prvHex, pubHex) =
      KeyPair
        (fromJust $ importPublicKey $ bytesFromHex $ T.pack pubHex)
        (fromJust $ importPrivateKey $ bytesFromHex $ T.pack prvHex)
    hexes =
      [ ( "00956fd0071623bf977b14304217c5d2464e6ef0b2258a5ebd5cdc7c05d2b29b26",
          "d1674834d2dc547614a0fafabfeea9f6c22e0564c8157ce533dccc5b72b2287603e74d7bd8a12ae707d0be5b85a8d53f14f9dc230a2eb77cdaa37f029de3c1ff"
        ),
        ( "00c30ab335bcd65ed030133ccecabc1715ec5672ffd250d2bbdea6457f1dc062b4",
          "e43a76ac275bb75c74d002742aaddb06f19ba6b3eaa40b25acd7d53cb319888be3ebd8423056f8faec726c203923a65fce908392e49f15e4cfd5e03169d03156"
        ),
        ( "0525e2ce8445da02339d2e811fd66904aa207fa9055485d83af7c091dff6cc01",
          "51a7ba8bea0128c02eb5fce1c7da78df62c5dccf5843bf19beb3f0c0a234d7fae3a44e83b4dfebb0e8d5ea5c45377985a7112e8317eb58f9a2dc7331eeb1b7ef"
        ),
        ( "0e826caf193e284e9f842875c8a9cba2a1bf3987a462c218170ef3afc1c763e8",
          "c3d700ffd79551d2cbf80666493c5ec3f50b1bf1b99ee8218f8eadb78d1e880afeffd810e328e7e461cde27c55986452d2868e29e4ac256ef1add5a9d9aa4c90"
        ),
        ( "00ee01e9cf4dfc3482a5e3e35591f8778399ab7b940225e95c09a2588505b85908",
          "b5f8dd1e374d23755df976058f1441b8fd4f8bcfa6b8a451b2a450562c647117aa852fa451613d4e09184919466dbb679b04322e00a961818e32df782ce7df48"
        )
      ]

publicKeys :: [PublicKey]
publicKeys = (\(KeyPair pub _) -> pub) <$> keyPairs

privateKeys :: [PrivateKey]
privateKeys = (\(KeyPair _ prv) -> prv) <$> keyPairs

powBlockHash1 :: PowBlockHash
powBlockHash1 = PowBlockHash $ bytesFromHex $ T.pack "e3ebd8423056f8faec726c203923a65fce908392e49f15e4cfd5e03169d03156"

powBlockHash2 :: PowBlockHash
powBlockHash2 = PowBlockHash $ bytesFromHex $ T.pack "d4f8bcfa6b8a451b2a450562c647117aa852fa451613d4e09184919466dbb679"

powBlockHash3 :: PowBlockHash
powBlockHash3 = PowBlockHash $ bytesFromHex $ T.pack "93c5ec3f50b1bf1b99ee8218f8eadb78d1e880afeffd810e328e7e461cde27c5"

makeBlockRef :: Int -> PowBlockHash -> PowBlockRef
makeBlockRef n = PowBlockRef (PowBlockNo n)

makeCheckpoint :: PowBlockRef -> [PrivateKey] -> Checkpoint
makeCheckpoint ref signers =
  Checkpoint ref signatures
  where
    signatures = (\sk -> fromJust $ sign sk bytes) <$> signers
    bytes = powBlockRefToBytes ref

makeVote :: PowBlockRef -> PrivateKey -> Vote
makeVote ref sk =
  Vote ref $ fromJust $ sign sk $ powBlockRefToBytes ref

makeVotes :: PowBlockRef -> [PrivateKey] -> [Vote]
makeVotes ref signers = makeVote ref <$> signers

makePoint :: Int -> Point TestBlock
makePoint slotNo =
  Point $ block (SlotNo $ fromIntegral slotNo) (castHash $ hash slotNo)

makeBlock :: Point TestBlock -> [Vote] -> TestBlock
makeBlock point votes =
  forgeMorpho testBftConfig (SlotNo slot) (BlockNo slot) (pointHash point) (mkMorphoGenTx . Tx <$> votes) ()
  where
    At (SlotNo slot') = pointSlot point
    slot = slot' + 1

testBftConfig :: ConsensusConfig (Bft ConsensusMockCrypto)
testBftConfig =
  BftConfig
    { bftParams = -- not used to forge blocks
        BftParams
          { bftSecurityParam = SecurityParam 4,
            bftNumNodes = NumCoreNodes 5
          },
      bftSignKey = SignKeyMockDSIGN 1,
      bftVerKeys = mempty -- not used to forge blocks
    }
