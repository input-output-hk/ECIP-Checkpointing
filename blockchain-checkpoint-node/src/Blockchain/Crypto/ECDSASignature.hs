{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}


-- | This module implements an ECDSA signature scheme based on SEC_p256k1 curve.
-- The point is to match the same signature scheme used in Blockchain, which enables public key recovery
-- (https://github.com/input-output-hk/blockchain/blob/develop/core/src/main/scala/io/iohk/ethereum/crypto/ECDSASignature.scala).
-- As in Blockchain the signature includes a recoveryId which can take the value of 27 or 28 (respectively for
-- negative and positive value of y coordinate of point R)
module Blockchain.Crypto.ECDSASignature (
  Signature(..),
  sign,
  recoverPublicKey,
  sigToHex,
  importPublicKey,
  importPrivateKey,
  PublicKey,
  PrivateKey,
  KeyPair(..),
  keyPairFromPrivate
) where

import Prelude hiding (fail)
import Cardano.Prelude (NoUnexpectedThunks)
import GHC.Generics (Generic)
import GHC.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BS (toShort, fromShort)
import Data.Aeson
import Control.Monad.Fail (fail)
import Data.Text (Text)
import Data.Maybe (fromJust)
import Codec.Serialise (Serialise (..))
import qualified Crypto.Secp256k1 as EC
import Crypto.Secp256k1 (SecKey, PubKey, RecSig, CompactRecSig (..))

import Blockchain.Common.Conversions
import qualified Blockchain.Common.Bytes as B

data PublicKey = PublicKey B.Bytes
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (Serialise)
  deriving (NoUnexpectedThunks)

data PrivateKey = PrivateKey B.Bytes
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (Serialise)
  deriving (NoUnexpectedThunks)


instance FromJSON PublicKey where
  parseJSON (String str) =
    case normalizeHex str of
      Just hex -> case importPublicKey $ bytesFromHex hex of
        Just sk -> pure sk
        Nothing -> fail $ "Invalid PublicKey (should be 64 bytes): " <> show str
      Nothing -> fail $  "Parsing of PublicKey failed. Invalid hex: " <> show str

  parseJSON invalid  = fail $  "Parsing of PublicKey failed due to type mismatch. "
                             <> "Encountered: " <> show invalid

instance FromJSON PrivateKey where
  parseJSON (String str) =
    case normalizeHex str of
      Just hex -> case importPrivateKey $ bytesFromHex hex of
        Just sk -> pure sk
        Nothing -> fail $ "Invalid PrivateKey (should be 32 bytes): " <> show str
      Nothing -> fail $  "Parsing of PrivateKey failed. Invalid hex: " <> show str

  parseJSON invalid  = fail $  "Parsing of PrivateKey failed due to type mismatch.  "
                             <> "Encountered: " <> show invalid

data Signature = Signature
    { sign_r :: B.Bytes -- ^ ECDSA r
    , sign_s :: B.Bytes -- ^ ECDSA s
    , sign_v :: Word8    -- recoveryId for Blockchain
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (Serialise)
      deriving (NoUnexpectedThunks)

data KeyPair = KeyPair PublicKey PrivateKey
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving (NoUnexpectedThunks)


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


blockchainRecIdOffset :: Word8
blockchainRecIdOffset = 27

uncompressedIndicator :: Word8
uncompressedIndicator = 4

recSigToSignature :: RecSig -> Signature
recSigToSignature rs =
  Signature (B.Bytes $ BS.fromShort r) (B.Bytes $ BS.fromShort s) (v + blockchainRecIdOffset)
  where
    CompactRecSig s r v = EC.exportCompactRecSig rs

recSigFromSignature :: Signature -> Maybe RecSig
recSigFromSignature sig =
  EC.importCompactRecSig compactRS
  where
    Signature (B.Bytes r) (B.Bytes s) v = sig
    compactRS = CompactRecSig (BS.toShort s) (BS.toShort r) (v - blockchainRecIdOffset)


-- | import a 32 or 33 byte long (that is with leading 0) private key
importPrivateKey :: B.Bytes -> Maybe PrivateKey
importPrivateKey (B.Bytes bytestr) =
  PrivateKey . B.Bytes . EC.getSecKey <$> EC.secKey adjustedBytestr
  where
    adjustedBytestr = if (BS.length bytestr > 32) then BS.drop 1 bytestr else bytestr

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



