{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Morpho.Ledger.Tx
  ( Tx (Tx),
    MorphoTxId,
  )
where

import Cardano.Crypto.Hash
import Cardano.Prelude
import Codec.Serialise (Serialise (..))
import Data.Aeson hiding (encode)
import Morpho.Ledger.PowTypes
import NoThunks.Class
import Ouroboros.Consensus.Util.Condense

newtype Tx = Tx Vote
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoThunks)

instance ToJSON Tx

type MorphoTxId = Hash Blake2b_256 Tx

instance Condense Tx where
  condense (Tx vote) = show vote
