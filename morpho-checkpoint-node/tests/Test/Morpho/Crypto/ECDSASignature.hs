{-# LANGUAGE OverloadedStrings #-}

module Test.Morpho.Crypto.ECDSASignature
  ( ecdsaTests,
  )
where

import qualified Data.ByteString as BS
import Data.Maybe (fromJust, isJust, isNothing)
import Morpho.Common.Bytes
import Morpho.Common.Conversions
import Morpho.Crypto.ECDSASignature
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

ecdsaTests :: TestTree
ecdsaTests =
  testGroup
    "ECDSA Signature tests"
    [ testCase "recoverable key" assert_morphoExample1,
      testCase "non-recoverable" assert_morphoExample2,
      testProperty "Random keypairs and messages" prop_randomKeyPairs
    ]

assert_morphoExample1 :: Assertion
assert_morphoExample1 =
  assertBool "Public key should be recoverable" $ isJust $ recoverPublicKey signature bytes
  where
    bytes = bytesFromHex "5a1465f4683bf2c18fc72c0789239c0f52b3ceac666ca9551cf265a11abe912c"
    r = bytesFromHex "f3af65a23fbf207b933d3c962381aa50e0ac19649c59c1af1655e592a8d95401"
    s = bytesFromHex "53629a403579f5ce57bcbefba2616b1c6156d308ddcd37372c94943fdabeda97"
    v = 28
    signature = Signature r s v

-- NOTE: I don't think this test makes any sense - (v = 31) doesn't make any sense. But for the sake of compliance with Morpho...
--       Nb. the key is recoverable with (v = 28)
assert_morphoExample2 :: Assertion
assert_morphoExample2 =
  assertBool "Public key should be non recoverable" $ isNothing $ recoverPublicKey signature bytes
  where
    bytes = bytesFromHex "2bb3925f178aa22c11435c61899e134fb7b1227016274b5f7b9d85c4469130ba"
    r = bytesFromHex "fbe3df0cf030655d817a89936850d1cc00c07c35d3b21be73cfe9a730ea8b753"
    s = bytesFromHex "62d73b6a92ac23ff514315fad795bbac6d485481d356329d71467e93c87dfa42"
    v = 31
    signature = Signature r s v

prop_randomKeyPairs :: Property
prop_randomKeyPairs =
  forAll generateKP $ \(KeyPair pk sk) ->
    forAll generateBytes $ \bytes ->
      let signature = sign sk bytes
          recoveredKey = signature >>= (\sig -> recoverPublicKey sig bytes)
       in recoveredKey == Just pk

generateKP :: Gen KeyPair
generateKP =
  keyPairFromPrivate <$> prvKey
  where
    prvKey = fromJust . importPrivateKey . integerToBytes 32 <$> d
    -- the `n` parameter of secp256k1 curve
    n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141 :: Integer
    d = choose (1, n - 1)

generateBytes :: Gen Bytes
generateBytes =
  Bytes . BS.pack <$> vectorOf 32 (choose (0, 255))
