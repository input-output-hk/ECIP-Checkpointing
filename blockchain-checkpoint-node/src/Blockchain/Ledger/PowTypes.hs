{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Blockchain.Ledger.PowTypes (
    PowBlockNo (..)
  , PowBlockHash (..)
  , PowBlockRef (..)
  , Vote (..)
  , Checkpoint (..)
  , genesisCheckpoint
  , powBlockRefToBytes
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (fail)
import Data.Aeson
import Cardano.Prelude (NoUnexpectedThunks)
import GHC.Generics (Generic)
import Codec.Serialise (Serialise (..))

import Blockchain.Crypto.ECDSASignature
import Blockchain.Common.Conversions
import Blockchain.Common.Bytes


newtype PowBlockNo = PowBlockNo Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Real, Enum, Integral)
  deriving anyclass (Serialise)
  deriving anyclass NoUnexpectedThunks

newtype PowBlockHash = PowBlockHash Bytes
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Serialise)
  deriving anyclass NoUnexpectedThunks

data PowBlockRef = PowBlockRef {
    powBlockNo :: PowBlockNo
  , powBlockHash :: PowBlockHash
} deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)
  deriving anyclass NoUnexpectedThunks

data Vote = Vote {
    votedPowBlock :: PowBlockRef
  , voteSignature :: Signature
} deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks


data Checkpoint = Checkpoint {
    checkpointedBlock :: PowBlockRef
  , chkpSignatures  :: [Signature]
} deriving stock    (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoUnexpectedThunks

genesisCheckpoint :: Checkpoint
genesisCheckpoint = Checkpoint (PowBlockRef (PowBlockNo 0) (PowBlockHash empty )) []

-- used for signing
powBlockRefToBytes :: PowBlockRef -> Bytes
powBlockRefToBytes (PowBlockRef (PowBlockNo _) (PowBlockHash h)) = h


instance FromJSON PowBlockHash where
  parseJSON (String text) =
    case normalizeHex text of
      Just h -> pure $ PowBlockHash $ bytesFromHex h
      Nothing -> fail $ "Failed to parse block hash. Invalid hash: " <> show text

  parseJSON invalid = fail $ "Failed to parse block hash due to type mismatch. Encountered: " <> show invalid

instance FromJSON PowBlockNo
