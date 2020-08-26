{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Blockchain.Common.Bytes (
  Bytes(..)
, empty
, length
) where

import Prelude hiding (length)
import Cardano.Prelude (NoUnexpectedThunks)
import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.HexString as Hex
import qualified Data.Text as T

newtype Bytes = Bytes { unBytes :: ByteString }
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise, NoUnexpectedThunks)

instance Show Bytes where
  show (Bytes bs) = T.unpack $ Hex.toText $ Hex.fromBytes bs


empty :: Bytes
empty = Bytes BS.empty

length :: Bytes -> Int
length (Bytes bs) = BS.length bs
