{-# LANGUAGE DeriveGeneric #-}
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
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Storage.ChainDB.API
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block hiding (Tip)

data TimeTravelError blk
  = LedgerStateNotFoundAt (Point blk)
  | ChainNotLongEnough Int Int
  deriving (Show, Generic)

getLatestStableLedgerState ::
  forall m blk.
  (MonadSTM m, LedgerSupportsProtocol blk) =>
  ChainDB m blk ->
  Int ->
  m (Either (TimeTravelError blk) (LedgerState blk))
getLatestStableLedgerState chainDB offset = atomically $ do
  anchoredFragment <- getCurrentChain chainDB
  let result = AF.head $ AF.dropNewest offset anchoredFragment
  case result of
    Left _ -> return $ Left $ ChainNotLongEnough offset (AF.length anchoredFragment)
    Right h -> getLedger h
  where
    getLedger hdr = do
      let pnt = blockPoint hdr
      mstate <- getPastLedger chainDB (castPoint pnt)
      case ledgerState <$> mstate of
        Nothing -> return $ Left $ LedgerStateNotFoundAt (castPoint pnt)
        Just st -> return $ Right st
