{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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
    Query,
    VoteError (..),
    WontPushCheckpoint (..),
    Ticked (..),
    voteBlockRef,
    genesisMorphoLedgerState,
    mkMorphoGenTx,
    updateMorphoState,
    updateMorphoLedgerState,
  )
where

import Cardano.Binary
import Cardano.Crypto.Hash
import Cardano.Prelude
import Codec.Serialise (Serialise (..))
import Data.Aeson hiding ((.:))
import qualified Data.Map as M
import Data.Text (pack)
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.State
import Morpho.Ledger.Tx
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.History.Summary (neverForksSummary)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Condense

newtype instance LedgerState (MorphoBlock h c) = MorphoLedgerState
  { morphoLedgerState :: MorphoState (MorphoBlock h c)
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (NoThunks, Serialise)

type instance LedgerCfg (LedgerState (MorphoBlock h c)) = MorphoLedgerConfig

-- | Set of constraints used by several Ledger-related
--   instances.
class
  ( Show (LedgerState (MorphoBlock h c)),
    Eq (LedgerState (MorphoBlock h c)),
    NoThunks (LedgerState (MorphoBlock h c)),
    NoThunks (LedgerCfg (LedgerState (MorphoBlock h c))),
    HashAlgorithm h,
    BftCrypto c
  ) =>
  MorphoStateDefaultConstraints h c

newtype instance Ticked (LedgerState (MorphoBlock h c))
  = MorphoTick (LedgerState (MorphoBlock h c))
  deriving (Generic)
  deriving anyclass (NoThunks)

instance GetTip (LedgerState (MorphoBlock h c)) where
  getTip = castPoint . morphoTip . morphoLedgerState

-- TODO: Why is this instance needed at all?
instance GetTip (Ticked (LedgerState (MorphoBlock h c))) where
  getTip (MorphoTick l) = castPoint $ getTip l

instance
  (MorphoStateDefaultConstraints h c) =>
  IsLedger (LedgerState (MorphoBlock h c))
  where
  type LedgerErr (LedgerState (MorphoBlock h c)) = MorphoError (MorphoBlock h c)

  applyChainTick _ _ = MorphoTick

instance
  (MorphoStateDefaultConstraints h c) =>
  ApplyBlock (LedgerState (MorphoBlock h c)) (MorphoBlock h c)
  where
  applyLedgerBlock = updateMorphoLedgerState

  reapplyLedgerBlock bcfg = (mustSucceed . runExcept) .: updateMorphoLedgerState bcfg
    where
      mustSucceed (Left err) = panic ("reapplyLedgerBlock: unexpected error: " <> show err)
      mustSucceed (Right st) = st

updateMorphoLedgerState ::
  ( MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c
  ) =>
  LedgerConfig blk ->
  MorphoBlock h c ->
  Ticked (LedgerState (MorphoBlock h c)) ->
  Except
    (MorphoError (MorphoBlock h c))
    (LedgerState (MorphoBlock h c))
updateMorphoLedgerState cfg b (MorphoTick (MorphoLedgerState st)) =
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

newtype instance TxId (GenTx (MorphoBlock h c)) = MorphoGenTxId
  {unMorphoGenTxId :: MorphoTxId}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise, NoThunks)

instance HasTxId (GenTx (MorphoBlock h c)) where
  txId = MorphoGenTxId . morphoGenTxId

instance ToJSON (TxId (GenTx (MorphoBlock h c))) where
  toJSON (MorphoGenTxId txid) = String . pack $ show txid

data instance GenTx (MorphoBlock h c) = MorphoGenTx
  { morphoGenTx :: !Tx,
    morphoGenTxId :: !MorphoTxId
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Serialise)

type instance ApplyTxErr (MorphoBlock h c) = (Vote, MorphoTransactionError)

instance
  ( HashAlgorithm h,
    BftCrypto c,
    UpdateLedger (MorphoBlock h c)
  ) =>
  LedgerSupportsMempool (MorphoBlock h c)
  where
  txInBlockSize _ = 2000 -- TODO: find something more suitable

  applyTx = applyTxMorpho
  reapplyTx = applyTxMorpho
  maxTxCapacity _ = 2000 -- TODO: calculate maxBlockSize - sizeHeaders

instance MorphoStateDefaultConstraints h c => UpdateLedger (MorphoBlock h c)

-- | TODO use proper values. The transactions and the header have fixed sizes
-- should be fixed and should be stored in the ledger state, where they will
-- be subject to UpdateProposals.
instance MorphoStateDefaultConstraints h c => CommonProtocolParams (MorphoBlock h c) where
  maxHeaderSize _ = maxBound
  maxTxSize _ = maxBound

-- Why is this needed if we're already updating the state in `updateMorphoState`???
applyTxMorpho ::
  forall h c.
  MorphoLedgerConfig ->
  SlotNo ->
  GenTx (MorphoBlock h c) ->
  Ticked (LedgerState (MorphoBlock h c)) ->
  Except (Vote, MorphoTransactionError) (Ticked (LedgerState (MorphoBlock h c)))
applyTxMorpho cfg _ tx (MorphoTick (MorphoLedgerState st)) =
  MorphoTick . MorphoLedgerState <$> stateAfterUpdate
  where
    (Tx v) = morphoGenTx tx
    stateAfterUpdate :: Except (Vote, MorphoTransactionError) (MorphoState (MorphoBlock h c))
    stateAfterUpdate = updateMorphoStateByVote cfg st v

instance (Typeable h, Typeable c) => NoThunks (GenTx (MorphoBlock h c)) where
  showTypeOf _ = show $ typeRep (Proxy @(GenTx (MorphoBlock h c)))

instance Condense (GenTx (MorphoBlock h c)) where
  condense = condense . morphoGenTx

instance Condense (GenTxId (MorphoBlock h c)) where
  condense = condense . unMorphoGenTxId

mkMorphoGenTx :: Tx -> GenTx (MorphoBlock h c)
mkMorphoGenTx tx =
  MorphoGenTx
    { morphoGenTx = tx,
      morphoGenTxId = hashWithSerialiser toCBOR tx
    }

updateMorphoState ::
  ( GetHeader blk,
    HashAlgorithm h,
    BftCrypto c,
    StandardHash blk,
    blk ~ MorphoBlock h c
  ) =>
  LedgerConfig blk ->
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
  if headerPrevHash hdr == pointHash tip'
    then pure $ MorphoState lc chAt vs (headerPoint hdr)
    else throwError $ MorphoInvalidHash (headerPrevHash hdr) (pointHash tip')

updateMorphoStateByTxs ::
  forall blk h c.
  (blk ~ MorphoBlock h c) =>
  MorphoLedgerConfig -> -- FullBlockConfig (LedgerState blk) blk ->
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
        Right s -> withExcept (uncurry MorphoTransactionError) $ updateMorphoStateByVote cfg s v
    mwinner :: Except (MorphoError blk) (Maybe PowBlockRef)
    mwinner = findWinner (requiredMajority cfg) <$> (M.elems . currentVotes <$> stateWithVotesApplied)
    stateVotes :: Except (MorphoError blk) [Vote]
    stateVotes = M.elems . currentVotes <$> stateWithVotesApplied
    votesForRef r (Vote r' _) = r == r'
    getSig (Vote _ s) = s

updateMorphoStateByVote ::
  MorphoLedgerConfig ->
  MorphoState blk ->
  Vote ->
  Except (Vote, MorphoTransactionError) (MorphoState blk)
updateMorphoStateByVote cfg st@(MorphoState lc chAt vs tip) v =
  (\xs -> MorphoState lc chAt xs tip) <$> updatedVotes
  where
    updatedVotes = withExceptT (v,) $ do
      distanceValid
      notDuplicate
      p <- recoveredPublicKey
      pkValid p
      pure $ M.insert p v vs
    distanceValid = isAtCorrectInterval cfg st (votedPowBlock v)
    notDuplicate = case find (== v) vs of
      Nothing -> return ()
      Just _ -> throwE MorphoDuplicateVote
    recoveredPublicKey = case recoverPublicKey (voteSignature v) (powBlockRefToBytes $ votedPowBlock v) of
      Nothing -> throwE MorphoInvalidSignature
      Just pk -> return pk
    pkValid p =
      if p `elem` fedPubKeys cfg
        then return ()
        else throwE MorphoUnknownPublicKey

isAtCorrectInterval :: MorphoLedgerConfig -> MorphoState blk -> PowBlockRef -> Except MorphoTransactionError ()
isAtCorrectInterval cfg st blockRef
  | bn < lcNo = throwE MorphoCandidateBeforeCheckpoint
  | bn == lcNo = throwE MorphoAlreadyCheckpointed
  | (bn - lcNo) `mod` interval /= 0 = throwE MorphoWrongDistance
  | otherwise = return ()
  where
    PowBlockNo lcNo = powBlockNo $ checkpointedBlock $ lastCheckpoint st
    PowBlockNo bn = powBlockNo blockRef
    interval = checkpointingInterval cfg

voteBlockRef :: MorphoLedgerConfig -> PowBlockRef -> Either VoteError Vote
voteBlockRef cfg ref = case sign sk bytes of
  Nothing -> Left $ FailedToSignBlockRef ref
  Just x -> Right (Vote ref x)
  where
    KeyPair _ sk = nodeKeyPair cfg
    bytes = powBlockRefToBytes ref

newtype VoteError = FailedToSignBlockRef PowBlockRef
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON)

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

data WontPushCheckpoint blk = WontPushCheckpoint (Point blk) (Point blk)
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
   Hard Fork History
-------------------------------------------------------------------------------}

instance HasHardForkHistory (MorphoBlock h c) where
  type HardForkIndices (MorphoBlock h c) = '[MorphoBlock h c]
  hardForkSummary cfg _st =
    neverForksSummary
      (EpochSize $ maxRollbacks (securityParam cfg) * 10)
      (slotLength cfg)

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

data instance Query (MorphoBlock h c) :: Type -> Type

deriving instance Show (Query (MorphoBlock h c) result)

instance SameDepIndex (Query (MorphoBlock h c)) where
  sameDepIndex query = case query of

instance ShowQuery (Query (MorphoBlock h c)) where
  showResult query = case query of

instance QueryLedger (MorphoBlock h c) where
  answerQuery _ query = case query of
