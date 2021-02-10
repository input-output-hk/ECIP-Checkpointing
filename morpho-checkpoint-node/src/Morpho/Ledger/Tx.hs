{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Morpho.Ledger.Tx
  ( Tx (Tx),
    MorphoTxId,
    InvalidTx (..),
  )
where

import NoThunks.Class
import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.Hash
import Cardano.Prelude
import Codec.Serialise (Serialise (..))
import Data.Aeson hiding (encode)
import GHC.Generics (Generic)
import Morpho.Ledger.PowTypes
import Ouroboros.Consensus.Util.Condense
import qualified Prelude as Prelude

data Tx = Tx Vote
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoThunks)

instance ToJSON Tx

{-# COMPLETE Tx #-}

type MorphoTxId = Hash Blake2b_256 Tx

data InvalidTx = InvalidTx Prelude.String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoThunks)

instance ToCBOR Tx where
  toCBOR = encode

instance Condense Tx where
  condense (Tx vote) = show vote
