{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}


module Blockchain.Ledger.Tx (
    Tx(Tx)
  , HasTxs(..)
  , TxId
  , InvalidTx(..)
  ) where

import           Prelude
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.Hash
import           Codec.Serialise (Serialise (..))
import           Ouroboros.Consensus.Util.Condense
import           Blockchain.Ledger.PowTypes

data Tx = Tx Vote
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks

{-# COMPLETE Tx #-}

type TxId  = Hash ShortHash Tx

data InvalidTx = InvalidTx String
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks

class HasTxs blk where
  getTxs :: blk -> [Tx]

instance ToCBOR Tx where
  toCBOR = encode

instance Condense Tx where
  condense (Tx vote) = show vote
