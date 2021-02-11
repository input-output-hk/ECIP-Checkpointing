{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

import NoThunks.Class
import Cardano.Prelude hiding (empty)
import Codec.Serialise (Serialise (..))
import Control.Monad.Fail (fail)
import Data.Aeson
import Morpho.Common.Bytes
import Morpho.Common.Conversions
import Morpho.Crypto.ECDSASignature

newtype PowBlockNo = PowBlockNo {unPowBlockNo :: Int}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Real, Enum, Integral, ToJSON, FromJSON)
  deriving anyclass (Serialise)
  deriving anyclass (NoThunks)

newtype PowBlockHash = PowBlockHash {unPowBlockHash :: Bytes}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Serialise)
  deriving anyclass (NoThunks)

instance FromJSON PowBlockHash where
  parseJSON (String text) =
    case normalizeHex text of
      Just h -> pure $ PowBlockHash $ bytesFromHex h
      Nothing -> fail $ "Failed to parse block hash. Invalid hash: " <> show text
  parseJSON invalid = fail $ "Failed to parse block hash due to type mismatch. Encountered: " <> show invalid

instance ToJSON PowBlockHash where
  toJSON (PowBlockHash bytes) = String $ bytesToHex bytes

data PowBlockRef = PowBlockRef
  { powBlockNo :: PowBlockNo,
    powBlockHash :: PowBlockHash
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)
  deriving anyclass (NoThunks)

instance ToJSON PowBlockRef where
  toJSON PowBlockRef {..} =
    object
      [ ("number", toJSON powBlockNo),
        ("hash", toJSON powBlockHash)
      ]

instance FromJSON PowBlockRef where
  parseJSON = withObject "PowBlockRef" $ \v ->
    PowBlockRef
      <$> v .: "number"
      <*> v .: "hash"

data Vote = Vote
  { votedPowBlock :: PowBlockRef,
    voteSignature :: Signature
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoThunks)

instance ToJSON Vote

data Checkpoint = Checkpoint
  { checkpointedBlock :: PowBlockRef,
    chkpSignatures :: [Signature]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise)
  deriving (NoThunks)

genesisCheckpoint :: Checkpoint
genesisCheckpoint = Checkpoint (PowBlockRef (PowBlockNo 0) (PowBlockHash empty)) []

-- used for signing
powBlockRefToBytes :: PowBlockRef -> Bytes
powBlockRefToBytes = unPowBlockHash . powBlockHash
