-- TODO: functions converting from string are partial but their types don't reflect that
-- currently I don't think it's worth changing that as these functions are used to read the config, and if it doesn't
-- parse correctly the node should crash anyway
-- We would need to change the lib (Data.HexString) or catch exeptions resulting in an unnecessary IO wrapping.
module Morpho.Common.Conversions
  ( integerFromHex,
    integerToHex,
    bytesToHex,
    bytesFromHex,
    normalizeHex,
    integerFromBytes,
    integerToBytes,
  )
where

import Cardano.Prelude
import qualified Data.ByteString as BS
import qualified Data.HexString as Hex
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Morpho.Common.Bytes as B

integerFromBytes :: (Integral a, Bits a) => B.Bytes -> a
integerFromBytes (B.Bytes bytestr) = BS.foldl' f 0 bytestr
  where
    f acc digit = acc `shiftL` 8 .|. fromIntegral digit

integerToBytes :: (Integral a, Bits a, Show a) => Int -> a -> B.Bytes
integerToBytes len n =
  if (BS.length padded > len)
    then panic $ "Integer " <> show n <> " to large to fit in " <> show len <> " bytes"
    else B.Bytes padded
  where
    padded = BS.append padding bytes
    bytes = i2b n len
    padding = BS.pack $ replicate padLen 0
    padLen = len - BS.length bytes
    i2b i k =
      if k > 0
        then BS.snoc (i2b remd (k - 1)) digit
        else BS.empty
      where
        digit = fromIntegral $ i .&. 0xFF
        remd = i `shiftR` 8

integerFromHexString :: (Integral a, Bits a) => Hex.HexString -> a
integerFromHexString = integerFromBytes . B.Bytes . Hex.toBytes

integerToHexString :: (Integral a, Bits a, Show a) => Int -> a -> Hex.HexString
integerToHexString len = Hex.fromBytes . B.unBytes . integerToBytes len

integerFromHex :: (Integral a, Bits a) => Text -> a
integerFromHex = integerFromHexString . Hex.hexString . encodeUtf8

integerToHex :: (Integral a, Bits a, Show a) => Int -> a -> Text
integerToHex len = Hex.toText . integerToHexString len

bytesToHex :: B.Bytes -> Text
bytesToHex = Hex.toText . Hex.fromBytes . B.unBytes

bytesFromHex :: Text -> B.Bytes
bytesFromHex = B.Bytes . Hex.toBytes . Hex.hexString . encodeUtf8

--validates hex, removes 0x prefix if present
normalizeHex :: Text -> Maybe Text
normalizeHex s =
  if validHex
    then Just normalized
    else Nothing
  where
    normalized = T.toLower $ if T.isPrefixOf "0x" s then T.drop 2 s else s
    hexChars = Set.fromList $ ['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F']
    inputAsSet = Set.fromList $ T.unpack normalized
    validChars = Set.difference inputAsSet hexChars == Set.empty
    validLength = T.length s `mod` 2 == 0
    validHex = validChars && validLength
