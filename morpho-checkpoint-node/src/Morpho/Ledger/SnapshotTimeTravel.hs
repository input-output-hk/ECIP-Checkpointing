{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Morpho.Ledger.SnapshotTimeTravel
  ( TimeTravelError (..),
    getLatestStableLedgerState,
  )
where

import Cardano.Prelude hiding (Handle, ReadMode, atomically, to, withFile)
import qualified Data.FingerTree.Strict as FT
import Morpho.Ledger.Block
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.ChainDB.API
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.AnchoredFragment (unanchorFragment)
import Ouroboros.Network.Block hiding (Tip)
import qualified Ouroboros.Network.ChainFragment as CF

data TimeTravelError blk
  = LedgerStateNotFoundAt (Point blk)
  | ChainNotLongEnough Int Int
  deriving (Show)

getLatestStableLedgerState ::
  (MonadIO m, MonadSTM m, HasHeader (Header blk)) =>
  ChainDB m blk ->
  Int ->
  m (Either (TimeTravelError blk) (LedgerState blk))
getLatestStableLedgerState chainDB offset = do
  chainFragment <- unanchorFragment <$> atomically (getCurrentChain chainDB)
  let result = CF.lookupByIndexFromEnd chainFragment offset
  case result of
    FT.Position _ h _ -> getLedger h
    _ -> return $ Left $ ChainNotLongEnough offset (CF.length chainFragment)
  where
    getLedger hdr = do
      let pnt = blockPoint hdr
      -- There is a race condition here. If the ledger db advances a lot between
      -- the two chainDB api calls, the ledger state will not be found and 'Nothing'
      -- will return. Newer ouroboros-consensus make past ledgerState available
      -- in STM transactions and can eliminate this race condition.
      mstate <- getPastLedger chainDB (castPoint pnt)
      case ledgerState <$> mstate of
        Nothing -> return $ Left $ LedgerStateNotFoundAt (castPoint pnt)
        Just st -> return $ Right st
