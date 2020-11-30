{-# LANGUAGE OverloadedStrings #-}

module Test.Morpho.MantisIntegration
  ( mantisIntegrationTests,
  )
where

import Cardano.Prelude
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Morpho.Common.Bytes
import Morpho.Common.Conversions
import Morpho.Crypto.ECDSASignature
import System.Process
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

mantisIntegrationTests :: TestTree
mantisIntegrationTests =
  testGroup
    "mantis-integration"
    [testProperty "Test the keygen against the actual Mantis parser" prop_mantisValidSig]

prop_mantisValidSig :: Property
prop_mantisValidSig =
  forAll generateKP $ \(KeyPair pk sk) ->
    forAll generateBytes $ \bytes -> ioProperty $ do
      let msignature = sign sk bytes
      r <- mantisValidateKey pk msignature bytes
      pure $ case r of
        Nothing -> succeeded
        Just err -> failed {reason = "Mantis returned a non 0 status: " <> T.unpack err}
  where
    mantisValidateKey :: PublicKey -> Maybe Signature -> Bytes -> IO (Maybe Text)
    mantisValidateKey _ Nothing h = pure . Just $ "Morpho cannot generate the signature for " <> bytesToHex h
    mantisValidateKey pk (Just s) h = do
      let hexs = T.unpack $ sigToHex s
          hexh = T.unpack $ bytesToHex h
          hexpk = T.unpack $ pubToHex pk
      (ec, _, err) <- readProcessWithExitCode "signatureValidator" [hexpk, hexs, hexh] ""
      pure $ case ec of
        ExitSuccess -> Nothing
        ExitFailure _ -> Just $ T.pack err

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
