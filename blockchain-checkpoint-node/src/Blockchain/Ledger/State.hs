{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Blockchain.Ledger.State (
    -- * State of the mock ledger
    MorphoState(..)
  , MorphoError(..)
  , MorphoLedgerConfig (..)
  , updateMorphoState
  , updateMorphoStateByTxs
  , updateMorphoStateByVote
  , considerCandidate
    -- * Genesis state
  , genesisMorphoState
  ) where

import           Cardano.Prelude

import           Codec.Serialise (Serialise)
import           Data.List (find, maximumBy)
import qualified Data.Map as M
import           Data.Map(Map)
import           Data.Ord (comparing)
import           Data.Tuple (swap)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block (ChainHash, HasHeader, HeaderHash,
                     Point, StandardHash, genesisPoint, pointHash)

import           Ouroboros.Consensus.Block

import           Blockchain.Ledger.Tx
import           Blockchain.Ledger.PowTypes
import           Blockchain.Crypto.ECDSASignature

{-------------------------------------------------------------------------------
  State of the ledger
-------------------------------------------------------------------------------}

--TODO: we should have a better way of passing the LedgerConfig to the state update function
-- a new version of ouroboros-network would allow this
-- particularly disconcerting is storing the private key here (although as long as the state is private it should be ok)
data MorphoState blk = MorphoState {
      lastCheckpoint :: !Checkpoint
    , checkpointAt   :: !(Point blk) -- when the checkpoint was created, used to determine if the checkpoint should be pushed to Blockchain
    , currentVotes   :: !(Map PublicKey Vote)
    , morphoTip      :: !(Point blk)
    , ledgerCfg      :: !MorphoLedgerConfig
    }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

deriving instance Serialise (HeaderHash blk) => Serialise (MorphoState blk)

data MorphoError blk =
    MorphoWrongDistance Vote
  | MorphoInvalidSignature Vote
  | MorphoDuplicateVote Vote
  | MorphoUnknownPublicKey Vote
  | MorphoInvalidHash (ChainHash blk) (ChainHash blk)
  deriving (Generic, NoUnexpectedThunks)


data MorphoLedgerConfig = MorphoLedgerConfig {
    checkpointingInterval :: Int
  , requiredMajority :: Int
  , fedPubKeys :: [PublicKey]
  , nodeKeyPair :: KeyPair
} deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoUnexpectedThunks)

-- WHAT'S THIS?
deriving instance StandardHash blk => Show (MorphoError blk)
deriving instance StandardHash blk => Eq   (MorphoError blk)
deriving instance Serialise (HeaderHash blk) => Serialise (MorphoError blk)

updateMorphoState :: ( Monad m
                  , GetHeader blk
                  , HasHeader (Header blk)
                  , StandardHash blk
                  , HasTxs blk
                  )
                => blk
                -> MorphoState blk
                -> ExceptT (MorphoError blk) m (MorphoState blk)
updateMorphoState b st = do
    st' <- updateMorphoTip (getHeader b) st
    updateMorphoStateByTxs (getTxs b) st'

updateMorphoTip :: (Monad m, HasHeader (Header blk), StandardHash blk)
              => Header blk
              -> MorphoState blk
              -> ExceptT (MorphoError blk) m (MorphoState blk)
updateMorphoTip hdr (MorphoState lc chAt vs tip' cfg) =
    if headerPrevHash hdr == pointHash tip'
      then pure $ MorphoState lc chAt vs (headerPoint hdr) cfg
      else throwError  $ MorphoInvalidHash (headerPrevHash hdr) (pointHash tip')

updateMorphoStateByTxs :: forall m blk. (Monad m)
                       => [Tx]
                       -> MorphoState blk
                       -> ExceptT (MorphoError blk) m (MorphoState blk)
updateMorphoStateByTxs txs st@(MorphoState _ _ _ tip cfg) = do
  winner <- lift $ runExceptT mwinner
  case winner of
    (Right (Just b)) -> (\chkp -> MorphoState chkp tip M.empty tip cfg) <$> makeCheckpoint b
    _ -> stateWithVotesApplied
  where
    makeCheckpoint :: PowBlockRef -> ExceptT (MorphoError blk) m Checkpoint
    makeCheckpoint b = Checkpoint b . map getSig . filter (votesForRef b) <$> stateVotes
    votesFromTxs :: [Vote]
    votesFromTxs = map (\(Tx v) -> v) txs
    stateWithVotesApplied :: ExceptT (MorphoError blk) m (MorphoState blk)
    stateWithVotesApplied = foldl' go (pure st) votesFromTxs
    go :: ExceptT (MorphoError blk) m (MorphoState blk) -> Vote -> ExceptT (MorphoError blk) m (MorphoState blk)
    go exs v = do
      es <- lift $ runExceptT exs
      case es of
        Left _ -> exs
        Right s -> updateMorphoStateByVote s v
    mwinner :: ExceptT (MorphoError blk) m (Maybe PowBlockRef)
    mwinner = findWinner (requiredMajority cfg) <$> (M.elems . currentVotes <$> stateWithVotesApplied)
    stateVotes :: ExceptT (MorphoError blk) m [Vote]
    stateVotes = M.elems . currentVotes <$> stateWithVotesApplied
    votesForRef r (Vote r' _) = r == r'
    getSig (Vote _ s) = s

updateMorphoStateByVote :: (Monad m) => MorphoState blk
                        -> Vote
                        -> ExceptT (MorphoError blk) m (MorphoState blk)
updateMorphoStateByVote st@(MorphoState lc chAt vs tip cfg) v =
  (\xs -> MorphoState lc chAt xs tip cfg) <$> updatedVotes
  where
    updatedVotes = do
      unless distanceValid . throwError . MorphoWrongDistance $ v
      unless notDuplicate . throwError . MorphoDuplicateVote $ v
      p <- recoveredKey
      unless (pkValid p) . throwError . MorphoUnknownPublicKey $ v
      pure $ M.insert p v vs
    notDuplicate = isNothing $ find (== v) vs
    distanceValid = isAtCorrectInterval st (votedPowBlock v)
    pkValid p = isJust $ find (p ==) (fedPubKeys cfg)
    maybeRecoveredKey = recoverPublicKey (voteSignature v) (powBlockRefToBytes $ votedPowBlock v)
    recoveredKey = maybe
      (throwError $ MorphoInvalidSignature v)
      pure
      maybeRecoveredKey


findWinner :: Int -> [Vote] -> Maybe PowBlockRef
findWinner _ [] = Nothing
findWinner m votes =
  if winnerCount < m
    then Nothing
    else Just winnerBlock
  where
    f (Vote blockRef _) occurrences = case M.lookup blockRef occurrences of
      Nothing -> M.insert blockRef 1 occurrences
      Just n  -> M.insert blockRef (n + 1) occurrences
    countVotes = M.toList $ foldr f M.empty votes
    (winnerBlock, winnerCount) = maximumBy (comparing swap) countVotes

considerCandidate :: MorphoState blk -> PowBlockRef -> Maybe Vote
considerCandidate st ref
  | isAtCorrectInterval st ref = do
      v <- Vote ref <$> sig
      if alreadyVoted st ref
        then Nothing
        else Just v
  | otherwise = Nothing
  where
    sig = sign sk bytes
    KeyPair _ sk = nodeKeyPair (ledgerCfg st)
    bytes = powBlockRefToBytes ref


isAtCorrectInterval :: MorphoState blk -> PowBlockRef -> Bool
isAtCorrectInterval (MorphoState
                      (Checkpoint
                        (PowBlockRef
                          (PowBlockNo lcNo)
                          _)
                        _)
                      _ _ _ cfg)
                    (PowBlockRef (PowBlockNo blockNo) _) =
  (blockNo - lcNo) `mod` (checkpointingInterval cfg) == 0 && blockNo > lcNo

alreadyVoted :: MorphoState blk -> PowBlockRef -> Bool
alreadyVoted st ref =
  lastVotedBlock == Just ref
  where
    lastVotedBlock = votedPowBlock <$> M.lookup pubKey (currentVotes st)
    KeyPair pubKey _ = nodeKeyPair (ledgerCfg st)

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

genesisMorphoState :: MorphoLedgerConfig -> MorphoState blk
genesisMorphoState cfg = MorphoState {
      lastCheckpoint  = genesisCheckpoint
    , checkpointAt    = genesisPoint
    , currentVotes    = M.empty
    , morphoTip       = genesisPoint
    , ledgerCfg       = cfg
    }
