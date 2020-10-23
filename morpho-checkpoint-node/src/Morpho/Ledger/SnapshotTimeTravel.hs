{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Morpho.Ledger.SnapshotTimeTravel
  ( TimeTravelError (..),
    getLatestStableLedgerState,
  )
where

import Cardano.Prelude hiding (Handle, ReadMode, atomically, to, withFile)
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.ChainDB.API
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.AnchoredFragment (anchorPoint)
import Ouroboros.Network.Block hiding (Tip)

newtype TimeTravelError blk = LedgerStateNotFoundAt (Point blk)
  deriving (Show)

getLatestStableLedgerState ::
  (MonadIO m, MonadSTM m) =>
  ChainDB m blk ->
  m (Either (TimeTravelError blk) (LedgerState blk))
getLatestStableLedgerState chainDB = go (5 :: Int)
  where
    go n = do
      immDbTip <- atomically $ castPoint . anchorPoint <$> getCurrentChain chainDB
      -- There is a race condition here. If the ledger db advances between the two
      -- calls, the ledger state will not be found and 'Nothing' will return.
      -- In this case we try again to get a stable ledger state and
      -- hopefully succeed this time.
      -- Newer ouroboros-consensus make past ledgerState available in STM
      -- transactions and can eliminate this race condition.
      mstate <- getPastLedger chainDB immDbTip
      case (ledgerState <$> mstate, n) of
        (Nothing, 0) -> return $ Left $ LedgerStateNotFoundAt immDbTip
        (Nothing, _) -> go $ n - 1
        (Just st, _) -> return $ Right st
