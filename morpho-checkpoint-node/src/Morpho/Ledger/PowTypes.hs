{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morpho.Ledger.PowTypes
  ( PowBlockNo (..),
    PowBlockHash (..),
    PowBlockRef (..),
    Vote (..),
    Checkpoint (..),
    genesisCheckpoint,
    powBlockRefToBytes,
  )
where

import Cardano.Prelude hiding (empty)
import Codec.Serialise (Serialise (..))
import Data.Aeson
import Morpho.Common.Bytes
import Morpho.Crypto.ECDSASignature
import NoThunks.Class

newtype PowBlockNo = PowBlockNo {unPowBlockNo :: Int}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Real, Enum, Integral, ToJSON)
  deriving anyclass (NoThunks, Serialise)

newtype PowBlockHash = PowBlockHash {unPowBlockHash :: Bytes}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON)
  deriving anyclass (NoThunks, Serialise)

data PowBlockRef = PowBlockRef
  { powBlockNo :: PowBlockNo,
    powBlockHash :: PowBlockHash
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, NoThunks, Serialise)

data Vote = Vote
  { votedPowBlock :: PowBlockRef,
    voteSignature :: Signature
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, NoThunks, Serialise)

data Checkpoint = Checkpoint
  { checkpointedBlock :: PowBlockRef,
    chkpSignatures :: [Signature]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, NoThunks, Serialise)

genesisCheckpoint :: Checkpoint
genesisCheckpoint = Checkpoint (PowBlockRef (PowBlockNo 0) (PowBlockHash empty)) []

-- used for signing
powBlockRefToBytes :: PowBlockRef -> Bytes
powBlockRefToBytes = unPowBlockHash . powBlockHash
