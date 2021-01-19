{-# LANGUAGE OverloadedStrings #-}

module Test.Morpho.Common.Utils
  ( utilsTestsIO,
  )
where

import Control.Exception (evaluate)
import qualified Data.Text as T
import qualified Morpho.Common.Bytes as B
import Morpho.Common.Conversions
import Test.Morpho.Common.Tables
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Prelude

utilsTestsIO :: IO TestTree
utilsTestsIO = do
  specs <- specTree
  return $
    testGroup "Utils tests" $
      specs
        <> [ testProperty "Round trip Int/Bytes/hex conversions (from Int)" prop_roundTripConversionsInt,
             testProperty "Round trip Int/Bytes/hex conversions (from Hex)" prop_roundTripConversionsHex
           ]
  where
    specTree =
      foldl (<>) (pure []) $
        testSpecs
          <$> [ spec_integerFromHex,
                spec_integerToHex,
                spec_bytesFromHex,
                spec_bytesToHex,
                spec_normalizeHex,
                spec_bytesShow
              ]

-- no explicit tests for integerFromBytes and integerToBytes as those are used by integerFromHex and integerToHex respectively,
-- and also tested in the round trip properties

spec_integerFromHex :: Spec
spec_integerFromHex =
  describe "integerFromHex" $ do
    byExample
      ("valid hex", "Int")
      [ ("1234", 0x1234),
        ("cafebabe", 0xcafebabe),
        ("0", 0),
        ("", 0),
        ("ffffffffffffffff", -1),
        ("ffffffff", 0xffffffff)
      ]
      (\h n -> integerFromHex h == (n :: Int))
    byExample
      ("valid hex", "Integer")
      [ ("1234", 0x1234),
        ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141",
          0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
        )
      ]
      (\h n -> integerFromHex h == (n :: Integer))
    byExample1Col
      "invalid hex resulting in error"
      ["hello", "0x12"]
      (\h -> evaluate (integerFromHex h :: Int) `shouldThrow` anyException)

spec_integerToHex :: Spec
spec_integerToHex =
  describe "integerToHex" $ do
    byExample
      ("Int", "converted to hex")
      [ (0x1234, "0000000000001234"),
        (0xdeadbeef, "00000000deadbeef"),
        (0, "0000000000000000"),
        (-1, "ffffffffffffffff")
      ]
      (\n h -> integerToHex 8 (n :: Int) == h)
    byExample
      ("Integer", "converted to hex")
      [ ( 0x1234,
          "0000000000000000000000000000000000000000000000000000000000001234"
        ),
        ( 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff,
          "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        ),
        ( -1,
          "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        )
      ]
      (\n h -> integerToHex 32 (n :: Integer) == h)

spec_bytesFromHex :: Spec
spec_bytesFromHex =
  describe "bytesFromHex" $ do
    byExample
      ("valid hex", "Bytes")
      [ ("cafebabe", [0xca, 0xfe, 0xba, 0xbe]),
        ("12121212121212121212121212121212", replicate 16 0x12),
        ("", [])
      ]
      (\h b -> bytesFromHex h == B.pack b)
    byExample1Col
      "invalid hex resulting in error"
      ["hello", "0xdeadbeef"]
      (\h -> evaluate (bytesFromHex h) `shouldThrow` anyException)

spec_bytesToHex :: Spec
spec_bytesToHex =
  describe "bytesToHex" $ do
    byExample
      ("valid hex", "Bytes")
      [ ([0xca, 0xfe, 0xba, 0xbe], "cafebabe"),
        (replicate 16 0x12, "12121212121212121212121212121212"),
        ([], "")
      ]
      (\b h -> bytesToHex (B.pack b) == h)

spec_normalizeHex :: Spec
spec_normalizeHex =
  describe "normalizeHex" $ do
    byExample
      ("hex", "result")
      [ ("", Just ""),
        ("0x", Just ""),
        ("abc", Nothing),
        ("hi", Nothing),
        ("cafe13", Just "cafe13"),
        ("0xcaFE13", Just "cafe13")
      ]
      (\h r -> normalizeHex h == r)

spec_bytesShow :: Spec
spec_bytesShow =
  describe "Bytes.show" $ do
    byExample
      ("Bytes", "String")
      [ (B.pack [], ""),
        (B.pack [0xca, 0xfe, 0xba, 0xbe], "cafebabe")
      ]
      (\b s -> show b == s)

prop_roundTripConversionsInt :: Property
prop_roundTripConversionsInt =
  forAll numbers $ \n ->
    roundTrip n == n
  where
    roundTrip :: Int -> Int
    roundTrip =
      integerFromBytes
        . bytesFromHex
        . (integerToHex 8 :: Int -> T.Text)
        . integerFromHex
        . bytesToHex
        . integerToBytes 8
    numbers = choose (minBound, maxBound)

prop_roundTripConversionsHex :: Property
prop_roundTripConversionsHex =
  forAll hexes $ \h ->
    roundTrip h == h
  where
    roundTrip :: T.Text -> T.Text
    roundTrip =
      integerToHex 8
        . (integerFromBytes :: B.Bytes -> Int)
        . bytesFromHex
        . bytesToHex
        . (integerToBytes 8 :: Int -> B.Bytes)
        . integerFromHex
    hexes = T.pack <$> vectorOf 16 (elements (['0' .. '9'] <> ['a' .. 'f']))
