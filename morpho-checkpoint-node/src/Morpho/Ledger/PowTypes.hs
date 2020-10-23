{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import Cardano.Prelude (NoUnexpectedThunks)
import Codec.Serialise (Serialise (..))
import Control.Monad.Fail (fail)
import Data.Aeson
import GHC.Generics (Generic)
import Morpho.Common.Bytes
import Morpho.Common.Conversions
import Morpho.Crypto.ECDSASignature

newtype PowBlockNo = PowBlockNo Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Real, Enum, Integral, ToJSON)
  deriving anyclass (Serialise)
  deriving anyclass (NoUnexpectedThunks)

newtype PowBlockHash = PowBlockHash Bytes
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Serialise)
  deriving anyclass (NoUnexpectedThunks)

instance ToJSON PowBlockHash where
  toJSON _ = Null

data PowBlockRef
  = PowBlockRef
      { powBlockNo :: PowBlockNo,
        powBlockHash :: PowBlockHash
      }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)
  deriving anyclass (NoUnexpectedThunks)

instance ToJSON PowBlockRef

data Vote
  = Vote
      { votedPowBlock :: PowBlockRef,
        voteSignature :: Signature
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoUnexpectedThunks)

instance ToJSON Vote

data Checkpoint
  = Checkpoint
      { checkpointedBlock :: PowBlockRef,
        chkpSignatures :: [Signature]
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise)
  deriving (NoUnexpectedThunks)

genesisCheckpoint :: Checkpoint
genesisCheckpoint = Checkpoint (PowBlockRef (PowBlockNo 0) (PowBlockHash empty)) []

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
