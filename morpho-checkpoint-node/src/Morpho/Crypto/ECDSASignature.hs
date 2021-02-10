{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module implements an ECDSA signature scheme based on SEC_p256k1 curve.
-- The point is to match the same signature scheme used in the PoW blockchain, which enables public key recovery
-- TODO: Add link.
-- As in the PoW blockchain the signature should include a recoveryId which can take the value of 27 or 28 (respectively for
-- negative and positive value of y coordinate of point R)
module Morpho.Crypto.ECDSASignature
  ( Signature (..),
    sign,
    recoverPublicKey,
    pubToHex,
    sigToHex,
    importPublicKey,
    importPrivateKey,
    PublicKey (..), -- not opaque for testing
    PrivateKey,
    KeyPair (..),
    keyPairFromPrivate,
  )
where

import NoThunks.Class
import Codec.Serialise (Serialise (..))
import Control.Monad.Fail (fail)
import Crypto.Secp256k1 (CompactRecSig (..), PubKey, RecSig, SecKey)
import qualified Crypto.Secp256k1 as EC
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BS (fromShort, toShort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Word (Word8)
import qualified Morpho.Common.Bytes as B
import Morpho.Common.Conversions
import Prelude hiding (fail)

newtype PublicKey = PublicKey B.Bytes
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (Serialise)
  deriving (NoThunks)

newtype PrivateKey = PrivateKey B.Bytes
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (Serialise)
  deriving (NoThunks)

instance FromJSON PublicKey where
  parseJSON (String str) =
    case normalizeHex str of
      Just hex -> case importPublicKey $ bytesFromHex hex of
        Just sk -> pure sk
        Nothing -> fail $ "Invalid PublicKey (should be 64 bytes): " <> show str
      Nothing -> fail $ "Parsing of PublicKey failed. Invalid hex: " <> show str
  parseJSON invalid =
    fail $
      "Parsing of PublicKey failed due to type mismatch. "
        <> "Encountered: "
        <> show invalid

instance FromJSON PrivateKey where
  parseJSON (String str) =
    case normalizeHex str of
      Just hex -> case importPrivateKey $ bytesFromHex hex of
        Just sk -> pure sk
        Nothing -> fail $ "Invalid PrivateKey (should be 32 bytes): " <> show str
      Nothing -> fail $ "Parsing of PrivateKey failed. Invalid hex: " <> show str
  parseJSON invalid =
    fail $
      "Parsing of PrivateKey failed due to type mismatch.  "
        <> "Encountered: "
        <> show invalid

data Signature = Signature
  { -- | ECDSA r
    sign_r :: B.Bytes,
    -- | ECDSA s
    sign_s :: B.Bytes,
    sign_v :: Word8 -- recoveryId for PoW BlockChain
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Serialise)
  deriving (NoThunks)

instance ToJSON Signature

data KeyPair = KeyPair
  { pKey :: PublicKey,
    sKey :: PrivateKey
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoThunks)

keyPairFromPrivate :: PrivateKey -> KeyPair
keyPairFromPrivate d = KeyPair (toPublicKey $ EC.derivePubKey $ getSecKey d) d

sign :: PrivateKey -> B.Bytes -> Maybe Signature
sign sk (B.Bytes msgBytestr) =
  recSigToSignature . EC.signRecMsg (getSecKey sk) <$> EC.msg msgBytestr

recoverPublicKey :: Signature -> B.Bytes -> Maybe PublicKey
recoverPublicKey sig (B.Bytes msgBytestr) = do
  m <- EC.msg msgBytestr
  s <- recSigFromSignature sig
  toPublicKey <$> EC.recover s m

morphoRecIdOffset :: Word8
morphoRecIdOffset = 27

uncompressedIndicator :: Word8
uncompressedIndicator = 4

recSigToSignature :: RecSig -> Signature
recSigToSignature rs =
  Signature (B.Bytes $ BS.fromShort r) (B.Bytes $ BS.fromShort s) (v + morphoRecIdOffset)
  where
    CompactRecSig r s v = EC.exportCompactRecSig rs

recSigFromSignature :: Signature -> Maybe RecSig
recSigFromSignature sig =
  EC.importCompactRecSig compactRS
  where
    Signature (B.Bytes r) (B.Bytes s) v = sig
    compactRS = CompactRecSig (BS.toShort r) (BS.toShort s) (v - morphoRecIdOffset)

-- | import a 32 or 33 byte long (that is with leading 0) private key
importPrivateKey :: B.Bytes -> Maybe PrivateKey
importPrivateKey (B.Bytes bytestr) =
  PrivateKey . B.Bytes . EC.getSecKey <$> EC.secKey adjustedBytestr
  where
    adjustedBytestr = if BS.length bytestr > 32 then BS.drop 1 bytestr else bytestr

-- | import an uncompressed 64 byte long public key (that is without compression indicator byte)
importPublicKey :: B.Bytes -> Maybe PublicKey
importPublicKey (B.Bytes bytestr) =
  toPublicKey <$> EC.importPubKey (BS.cons uncompressedIndicator bytestr)

-- safe if PrivateKey was created using importPrivateKey
getSecKey :: PrivateKey -> SecKey
getSecKey (PrivateKey (B.Bytes bytestr)) = fromJust $ EC.secKey bytestr

toPublicKey :: PubKey -> PublicKey
toPublicKey = PublicKey . B.Bytes . BS.tail . EC.exportPubKey False

sigToHex :: Signature -> Text
sigToHex (Signature r s v) = bytesToHex r <> bytesToHex s <> integerToHex 1 v

pubToHex :: PublicKey -> Text
pubToHex (PublicKey b) = bytesToHex b
