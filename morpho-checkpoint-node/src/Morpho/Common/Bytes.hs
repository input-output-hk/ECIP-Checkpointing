{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morpho.Common.Bytes
  ( Bytes (..),
    empty,
    length,
    pack,
    unpack,
  )
where

import Codec.Serialise (Serialise)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class
import Prelude hiding (length)

newtype Bytes = Bytes {unBytes :: ByteString}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving anyclass (Serialise, NoThunks)

instance Show Bytes where
  show (Bytes bs) = T.unpack $ TE.decodeUtf8 $ B16.encode bs

instance ToJSON Bytes where
  toJSON = toJSON . show

instance Semigroup Bytes where
  (<>) (Bytes b1) (Bytes b2) = Bytes $ b1 <> b2

instance Monoid Bytes where
  mempty = empty

empty :: Bytes
empty = Bytes BS.empty

length :: Bytes -> Int
length (Bytes bs) = BS.length bs

pack :: [Word8] -> Bytes
pack = Bytes . BS.pack

unpack :: Bytes -> [Word8]
unpack = BS.unpack . unBytes
