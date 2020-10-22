{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- We're going to define several ouroboros-network related orphan
-- instances in this module.

module Morpho.Ledger.Update
  ( TxId (..),
    GenTx (..),
    LedgerState (..),
    MorphoStateDefaultConstraints,
    Query (..),
    considerCandidate,
    genesisMorphoLedgerState,
    mkMorphoGenTx,
    updateMorphoState,
    updateMorphoLedgerState,
  )
where

import Cardano.Crypto.Hash
import Cardano.Prelude
import Codec.Serialise (Serialise (..))
import Data.Aeson hiding ((.:))
import Data.List (find, maximumBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Text (pack)
import Data.Tuple (swap)
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.State
import Morpho.Ledger.Tx
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.History.EraParams (defaultEraParams)
import Ouroboros.Consensus.HardFork.History.Summary (neverForksSummary)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Condense

newtype instance LedgerState (MorphoBlock h c)
  = MorphoLedgerState
      { morphoLedgerState :: MorphoState (MorphoBlock h c)
      }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NoUnexpectedThunks, Serialise)

type instance LedgerCfg (LedgerState (MorphoBlock h c)) = MorphoLedgerConfig

-- | Set of constraints used by several Ledger-related
--   instances.
class
  ( Show (LedgerState (MorphoBlock h c)),
    Eq (LedgerState (MorphoBlock h c)),
    NoUnexpectedThunks (LedgerState (MorphoBlock h c)),
    NoUnexpectedThunks (LedgerCfg (LedgerState (MorphoBlock h c))),
    HashAlgorithm h,
    BftCrypto c
  ) =>
  MorphoStateDefaultConstraints h c

instance
  (MorphoStateDefaultConstraints h c) =>
  IsLedger (LedgerState (MorphoBlock h c))
  where
  type LedgerErr (LedgerState (MorphoBlock h c)) = MorphoError (MorphoBlock h c)

  applyChainTick _ = Ticked

  ledgerTipPoint = castPoint . morphoTip . morphoLedgerState

instance
  (MorphoStateDefaultConstraints h c) =>
  ApplyBlock (LedgerState (MorphoBlock h c)) (MorphoBlock h c)
  where
  applyLedgerBlock bcfg = updateMorphoLedgerState (blockConfigLedger bcfg)

  reapplyLedgerBlock bcfg = (mustSucceed . runExcept) .: updateMorphoLedgerState (blockConfigLedger bcfg)
    where
      mustSucceed (Left err) = panic ("reapplyLedgerBlock: unexpected error: " <> show err)
      mustSucceed (Right st) = st

updateMorphoLedgerState ::
  (MorphoStateDefaultConstraints h c) =>
  MorphoLedgerConfig ->
  MorphoBlock h c ->
  Ticked (LedgerState (MorphoBlock h c)) ->
  Except (MorphoError (MorphoBlock h c))
    (LedgerState (MorphoBlock h c))
updateMorphoLedgerState cfg b (Ticked _ (MorphoLedgerState st)) =
  MorphoLedgerState <$> updateMorphoState cfg b st

genesisMorphoLedgerState :: LedgerState (MorphoBlock h c)
genesisMorphoLedgerState = MorphoLedgerState genesisMorphoState

instance HasTxs (MorphoBlock h c) where
  extractTxs b = extractTx <$> blockTxs
    where
      blockTxs = morphoTxs $ morphoBody b
      extractTx btx =
        MorphoGenTx
          { morphoGenTx = morphoBlockGenTx btx,
            morphoGenTxId = morphoBlockGenTxId btx
          }

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

instance HasTxId (GenTx (MorphoBlock h c)) where
  data TxId (GenTx (MorphoBlock h c))
    = MorphoGenTxId
        {unMorphoGenTxId :: !MorphoTxId}
    deriving (Show, Eq, Ord, Generic, Serialise, NoUnexpectedThunks)

  txId = MorphoGenTxId . morphoGenTxId

instance ToJSON (TxId (GenTx (MorphoBlock h c))) where
  toJSON (MorphoGenTxId txid) = String . pack $ show txid

instance
  ( HashAlgorithm h,
    Typeable c,
    UpdateLedger (MorphoBlock h c)
  ) =>
  LedgerSupportsMempool (MorphoBlock h c)
  where
  data GenTx (MorphoBlock h c)
    = MorphoGenTx
        { morphoGenTx :: !Tx,
          morphoGenTxId :: !MorphoTxId
        }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (Serialise)

  txInBlockSize _ = 2000 -- TODO: find something more suitable

  type ApplyTxErr (MorphoBlock h c) = MorphoError (MorphoBlock h c)

  applyTx = applyTxMorpho
  reapplyTx = applyTxMorpho
  maxTxCapacity = \_ -> 2000 -- TODO: calculate maxBlockSize - sizeHeaders

instance MorphoStateDefaultConstraints h c => UpdateLedger (MorphoBlock h c)

-- | TODO use proper values. The transactions and the header have fixed sizes
-- should be fixed and should be stored in the ledger state, where they will
-- be subject to UpdateProposals.
instance MorphoStateDefaultConstraints h c => CommonProtocolParams (MorphoBlock h c) where
  maxHeaderSize _ = maxBound
  maxTxSize _ = maxBound

-- Why is this needed if we're already updating the state in `updateMorphoState`???
applyTxMorpho ::
  forall blk h c.
  (blk ~ MorphoBlock h c) =>
  MorphoLedgerConfig ->
  GenTx (MorphoBlock h c) ->
  TickedLedgerState (MorphoBlock h c) ->
  Except (MorphoError blk) (TickedLedgerState (MorphoBlock h c))
applyTxMorpho cfg tx (Ticked slotNo (MorphoLedgerState st)) =
  Ticked slotNo . MorphoLedgerState <$> stateAfterUpdate
  where
    (Tx v) = morphoGenTx tx
    stateAfterUpdate :: Except (MorphoError blk) (MorphoState (MorphoBlock h c))
    stateAfterUpdate = updateMorphoStateByVote cfg st v

instance (Typeable h, Typeable c) => NoUnexpectedThunks (GenTx (MorphoBlock h c)) where
  showTypeOf _ = show $ typeRep (Proxy @(GenTx (MorphoBlock h c)))

instance Condense (GenTx (MorphoBlock h c)) where
  condense = condense . morphoGenTx

instance Condense (GenTxId (MorphoBlock h c)) where
  condense = condense . unMorphoGenTxId

mkMorphoGenTx :: Tx -> GenTx (MorphoBlock h c)
mkMorphoGenTx tx =
  MorphoGenTx
    { morphoGenTx = tx,
      morphoGenTxId = hash tx
    }

updateMorphoState ::
  ( GetHeader blk,
    HashAlgorithm h,
    BftCrypto c,
    StandardHash blk,
    blk ~ MorphoBlock h c
  ) =>
  MorphoLedgerConfig ->
  MorphoBlock h c ->
  MorphoState blk ->
  Except (MorphoError blk) (MorphoState blk)
updateMorphoState cfg b st = do
  st' <- updateMorphoTip (getHeader b) st
  updateMorphoStateByTxs cfg (morphoGenTx <$> extractTxs b) st'

updateMorphoTip ::
  ( HasHeader (Header blk),
    StandardHash blk,
    HashAlgorithm h,
    BftCrypto c,
    blk ~ MorphoBlock h c
  ) =>
  Header (MorphoBlock h c) ->
  MorphoState blk ->
  Except (MorphoError blk) (MorphoState blk)
updateMorphoTip hdr (MorphoState lc chAt vs tip') =
  if (headerPrevHash defaultCodecConfig hdr) == pointHash tip'
    then pure $ MorphoState lc chAt vs (headerPoint hdr)
    else throwError $ MorphoInvalidHash (headerPrevHash defaultCodecConfig hdr) (pointHash tip')

updateMorphoStateByTxs ::
  forall blk h c.
  (blk ~ MorphoBlock h c) =>
  MorphoLedgerConfig ->
  [Tx] ->
  MorphoState blk ->
  Except (MorphoError blk) (MorphoState blk)
updateMorphoStateByTxs cfg txs st@(MorphoState _ _ _ tip) = do
  winner <- lift $ runExceptT mwinner
  case winner of
    (Right (Just b)) -> (\chkp -> MorphoState chkp tip M.empty tip) <$> makeCheckpoint b
    _ -> stateWithVotesApplied
  where
    makeCheckpoint :: PowBlockRef -> Except (MorphoError blk) Checkpoint
    makeCheckpoint b = Checkpoint b . map getSig . filter (votesForRef b) <$> stateVotes
    votesFromTxs :: [Vote]
    votesFromTxs = map (\(Tx v) -> v) txs
    stateWithVotesApplied :: Except (MorphoError blk) (MorphoState blk)
    stateWithVotesApplied = foldl' go (pure st) votesFromTxs
    go :: Except (MorphoError blk) (MorphoState blk) -> Vote -> Except (MorphoError blk) (MorphoState blk)
    go exs v = do
      es <- lift $ runExceptT exs
      case es of
        Left _ -> exs
        Right s -> updateMorphoStateByVote cfg s v
    mwinner :: Except (MorphoError blk) (Maybe PowBlockRef)
    mwinner = findWinner (requiredMajority cfg) <$> (M.elems . currentVotes <$> stateWithVotesApplied)
    stateVotes :: Except (MorphoError blk) [Vote]
    stateVotes = M.elems . currentVotes <$> stateWithVotesApplied
    votesForRef r (Vote r' _) = r == r'
    getSig (Vote _ s) = s

updateMorphoStateByVote ::
  (blk ~ MorphoBlock h c) =>
  MorphoLedgerConfig ->
  MorphoState blk ->
  Vote ->
  Except (MorphoError blk) (MorphoState blk)
updateMorphoStateByVote cfg st@(MorphoState lc chAt vs tip) v =
  (\xs -> MorphoState lc chAt xs tip) <$> updatedVotes
  where
    updatedVotes = do
      unless distanceValid . throwError . MorphoWrongDistance $ v
      unless notDuplicate . throwError . MorphoDuplicateVote $ v
      p <- recoveredPoint
      unless (pkValid p) . throwError . MorphoUnknownPublicKey $ v
      pure $ M.insert p v vs
    notDuplicate = isNothing $ find (== v) vs
    distanceValid = isAtCorrectInterval cfg st (votedPowBlock v)
    pkValid p = isJust $ find (p ==) (fedPubKeys cfg)
    maybeRecoveredPoint = recoverPublicKey (voteSignature v) (powBlockRefToBytes $ votedPowBlock v)
    recoveredPoint =
      maybe
        (throwError $ MorphoInvalidSignature v)
        pure
        maybeRecoveredPoint

isAtCorrectInterval :: MorphoLedgerConfig -> MorphoState blk -> PowBlockRef -> Bool
isAtCorrectInterval cfg st blockRef =
  (bn - lcNo) `mod` (checkpointingInterval cfg) == 0 && bn > lcNo
  where
    PowBlockNo lcNo = powBlockNo $ checkpointedBlock $ lastCheckpoint st
    PowBlockNo bn = powBlockNo blockRef

considerCandidate :: MorphoLedgerConfig -> MorphoState blk -> PowBlockRef -> Maybe Vote
considerCandidate cfg st ref
  | isAtCorrectInterval cfg st ref = do
    v <- Vote ref <$> sig
    if alreadyVoted cfg st ref
      then Nothing
      else Just v
  | otherwise = Nothing
  where
    sig = sign sk bytes
    KeyPair _ sk = nodeKeyPair cfg
    bytes = powBlockRefToBytes ref

findWinner :: Int -> [Vote] -> Maybe PowBlockRef
findWinner _ [] = Nothing
findWinner m votes =
  if winnerCount < m
    then Nothing
    else Just winnerBlock
  where
    f (Vote blockRef _) occurrences = case M.lookup blockRef occurrences of
      Nothing -> M.insert blockRef 1 occurrences
      Just n -> M.insert blockRef (n + 1) occurrences
    countVotes = M.toList $ foldr f M.empty votes
    (winnerBlock, winnerCount) = maximumBy (comparing swap) countVotes

alreadyVoted :: MorphoLedgerConfig -> MorphoState blk -> PowBlockRef -> Bool
alreadyVoted cfg st ref = lastVotedBlock == Just ref
  where
    lastVotedBlock = votedPowBlock <$> M.lookup pubKey (currentVotes st)
    KeyPair pubKey _ = nodeKeyPair cfg

{-------------------------------------------------------------------------------
   Hard Fork History
-------------------------------------------------------------------------------}

instance HasHardForkHistory (MorphoBlock h c) where
  type HardForkIndices (MorphoBlock h c) = '[MorphoBlock h c]
  hardForkSummary cfg _st =
    neverForksSummary
      (defaultEraParams defaultSecurityParam (slotLength cfg))

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

instance QueryLedger (MorphoBlock h c) where
  data Query (MorphoBlock h c) :: Type -> Type where
    GetDummy :: Query (MorphoBlock h c) ()
  answerQuery _ GetDummy _ = ()
  eqQuery GetDummy GetDummy = Just Refl

deriving instance Show (Query (MorphoBlock h c) result)

instance ShowQuery (Query (MorphoBlock h c)) where
  showResult GetDummy = show
