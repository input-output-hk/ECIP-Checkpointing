{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Blockchain.Ledger.SnapshotTimeTravel (
  LedgerStateTimeTravelError(..),
  getLatestStableLedgerState
) where

import Cardano.Prelude hiding (atomically, to, withFile, ReadMode, Handle)
import Prelude (String)

import Codec.Serialise.Decoding (Decoder)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Util.CBOR hiding (Decoder, readIncremental)
import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Read as CBOR
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.ResourceRegistry
import Ouroboros.Network.Block hiding (Tip)
import Ouroboros.Network.AnchoredFragment (anchorPoint)
import Ouroboros.Storage.ChainDB.API
import Ouroboros.Storage.Common
import Ouroboros.Storage.FS.API
import Ouroboros.Storage.LedgerDB.InMemory
import Ouroboros.Storage.FS.API.Types

import Blockchain.Ledger.State
import Blockchain.Ledger.Block

data LedgerStateTimeTravelError c ext where
  LedgerUpdateError :: StandardHash blk => MorphoError blk -> LedgerStateTimeTravelError c ext
  SomeBlockHaveBeenGCed :: HeaderHash (MorphoBlock c ext) -> LedgerStateTimeTravelError c ext
  BlockReadError :: ReadIncrementalErr -> LedgerStateTimeTravelError c ext
  StreamUnknownRange ::  StandardHash blk => UnknownRange blk -> LedgerStateTimeTravelError c ext
  StreamInvalidRange ::  LedgerStateTimeTravelError c ext
  MissingImmDbTip :: LedgerStateTimeTravelError c ext
  SnapshotReadError :: ReadIncrementalErr -> LedgerStateTimeTravelError c ext

deriving instance Show (LedgerStateTimeTravelError c ext)

-- | Get the ledger state associated with the latest stable block.
--
-- This function is a dirty hack, it will:
-- 1. Retrieve the latest snapshot from the DB.
-- 2. Extract the ledger state from this snapshot.
-- 3. Stream all the blocks between the latest snapshot and the tip of
--    the stable chain.
-- 4. Return the ledger state associated with the tip of the stable
--    chain.
--
getLatestStableLedgerState :: forall m l h blk c ext.
  ((l ~ LedgerState blk), (MorphoCrypto c), (Typeable ext), (IOLike m), (MorphoBlock c ext) ~ blk)
  => HasFS m h
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (Point blk))
  -> ResourceRegistry m
  -> ChainDB m blk
  -> LedgerState blk
  -> ExceptT (LedgerStateTimeTravelError c ext) m (LedgerState blk)
getLatestStableLedgerState hasFs decLedger decRef registry chainDB genesisLS = do
  it <- getIterator
  sst <- snapshotState
  go it sst
  where
    getIterator :: ExceptT (LedgerStateTimeTravelError c ext) m (Iterator m (Deserialisable m blk blk))
    getIterator = do
      fr <- snapshotTip
      to <- lift immDbTip
      if to <= fr then
        throwError StreamInvalidRange
      else
        withExceptT StreamUnknownRange . ExceptT $ streamBlocks chainDB registry (StreamFromExclusive fr) (StreamToInclusive to)

    go :: Iterator m (Deserialisable m blk blk) -> LedgerState blk -> ExceptT (LedgerStateTimeTravelError c ext) m (LedgerState blk)
    go it st = do
      next <- lift $ iteratorNext it
      case next of
        IteratorExhausted -> pure st
        (IteratorResult sb)  -> do
          b <- lift $ deserialise sb
          res <- runExceptT $ updateMorphoLedgerState b st
          case res of 
            Left err -> throwError $ LedgerUpdateError err
            Right st' -> go it st'
        (IteratorBlockGCed hash) -> throwError $ SomeBlockHaveBeenGCed hash
    mdSnap = do
      snps <- listSnapshots hasFs
      pure $ latestSnapshot snps
    esnapshot :: ExceptT ReadIncrementalErr m (ChainSummary l (Point blk))
    esnapshot = do
      maybeDSnap <- lift mdSnap
      case maybeDSnap of 
        Just dSnap -> readSnapshot hasFs decLedger decRef dSnap
        Nothing -> pure $ genesisSummary genesisLS
    snapshotState :: ExceptT (LedgerStateTimeTravelError c ext) m l
    snapshotState = withExceptT SnapshotReadError (csLedger <$> esnapshot)
    immDbTip :: m (Point blk)
    immDbTip = atomically $ castPoint . anchorPoint <$> getCurrentChain chainDB
    snapshotTip :: ExceptT (LedgerStateTimeTravelError c ext) m (Point blk)
    snapshotTip = withExceptT SnapshotReadError $ tipToPoint . csTip <$> esnapshot

-- The following code is vendored from
-- Ouroboros.Storage.LedgerDB.OnDisk
--
-- For this trick to work, we need to acces some ouroboros-network
-- internal functions that are sadly not currently exposed.
-- ===============================================

-- | Read snapshot from disk
readSnapshot :: forall m l r h. (IOLike m)
             => HasFS m h
             -> (forall s. Decoder s l)
             -> (forall s. Decoder s r)
             -> DiskSnapshot
             -> ExceptT ReadIncrementalErr m (ChainSummary l r)
readSnapshot hasFS decLedger decRef =
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToPath
  where
    decoder :: Decoder s (ChainSummary l r)
    decoder = decodeChainSummary decLedger decRef

listSnapshots :: Monad m => HasFS m h -> m [DiskSnapshot]
listSnapshots HasFS{..} =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortBy (flip compare) . mapMaybe snapshotFromPath . Set.toList

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath = fmap DiskSnapshot . readMaybe

latestSnapshot :: [DiskSnapshot] -> Maybe DiskSnapshot
latestSnapshot [] = Nothing
latestSnapshot ss = Just $ maximum ss

genesisSummary :: LedgerState blk -> ChainSummary (LedgerState blk) (Point blk)
genesisSummary genesisState = ChainSummary TipGen 0 genesisState

-- | On disk snapshots are numbered monotonically
newtype DiskSnapshot = DiskSnapshot Int
  deriving (Show, Eq, Ord, Generic)

snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath (DiskSnapshot ss) = mkFsPath [show ss]


-- The following code is vendored from
-- Ouroboros.Consensus.Util.CBOR 
-- ===============================================

-- | Read a file incrementally
--
-- NOTE: The 'MonadThrow' constraint is only needed for 'bracket'. This
-- function does not actually throw anything.
--
-- NOTE: This uses a chunk size of roughly 32k. If we use this function to read
-- small things this might not be ideal.
--
-- NOTE: This currently expects the file to contain precisely one value; see also
-- 'readIncrementalOffsets'.
readIncremental :: forall m h a. IOLike m
                => HasFS m h
                -> (forall s . CBOR.Decoder s a)
                -> FsPath
                -> m (Either ReadIncrementalErr a)
readIncremental hasFS@HasFS{..} decoder fp = withLiftST $ \liftST -> do
    withFile hasFS fp ReadMode $ \h ->
      go liftST h =<< liftST (CBOR.deserialiseIncremental decoder)
  where
    go :: (forall x. ST s x -> m x)
       -> Handle h
       -> CBOR.IDecode s a
       -> m (Either ReadIncrementalErr a)
    go liftST h (CBOR.Partial k) = do
        bs   <- hGetSome h (fromIntegral defaultChunkSize)
        dec' <- liftST $ k (checkEmpty bs)
        go liftST h dec'
    go _ _ (CBOR.Done _ _ a) =
        return $ Right a
    go _ _ (CBOR.Fail _ _ err) =
        return $ Left $ ReadFailed err

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs
