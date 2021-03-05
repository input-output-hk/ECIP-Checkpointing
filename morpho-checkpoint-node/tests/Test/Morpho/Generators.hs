{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.Generators where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Codec.CBOR.Write
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Short as SB
import Data.Proxy
import qualified Data.Text as T
import Morpho.Common.Bytes
import Morpho.Common.Conversions
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.Block
import Morpho.Ledger.PowTypes
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.State
import Morpho.Ledger.Tx
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.HeaderValidation (AnnTip (..))
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Node.Serialisation
import Ouroboros.Consensus.Protocol.BFT
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Instances ()
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Orphans.Slotting.Arbitrary ()
import Test.Util.Serialisation.Roundtrip
import Prelude

type TestBlock = MorphoBlock MorphoMockHash ConsensusMockCrypto

type TestStdHeader = MorphoStdHeader MorphoMockHash ConsensusMockCrypto

instance Arbitrary TestBlock where
  arbitrary :: Gen TestBlock
  arbitrary = do
    headerStd <- arbitrary
    body <- arbitrary
    -- FIXME: Implement this in the actual block forging
    let bftFields = forgeBftFields testBftConfig headerStd
        ccfg :: CodecConfig TestBlock
        ccfg = MorphoCodecConfig ()

        -- Since the block contains an estimate of its own size (morphoBlockSize)
        -- to get its size we first assume a size of 0
        minHeader = mkMorphoHeader headerStd bftFields 0
        minBlock = MorphoBlock minHeader body
        minBlockSize = fromIntegral $ BS.length $ toLazyByteString $ encodeNodeToNode ccfg MorphoNodeToNodeVersion1 minBlock

        -- However because Word64 has a variable length, we can still
        -- underestimate the size, which is not good because morphoBlockSize's
        -- purpose is for an *overestimate* for estimateBlockSize. So we add
        -- 10 extra bytes, which is just the amount we can overestimate in the
        -- estimateBlockSize test, and more than it could actually vary due to
        -- Word64's encoding
        header = mkMorphoHeader headerStd bftFields (minBlockSize + 10)
        block = MorphoBlock header body
    return block

instance Arbitrary (Header TestBlock) where
  arbitrary = do
    headerStd <- arbitrary
    size <- arbitrary
    -- Do we want more randomness here, even if the block becomes
    -- invalid?
    let bftFields = forgeBftFields testBftConfig headerStd
    return $ mkMorphoHeader headerStd bftFields size

instance Arbitrary TestStdHeader where
  arbitrary =
    MorphoStdHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary MorphoBody where
  arbitrary = MorphoBody <$> listOf arbitrary

instance Arbitrary MorphoBlockTx where
  arbitrary = do
    MorphoGenTx {..} <- arbitrary @(GenTx TestBlock)
    return $ MorphoBlockTx morphoGenTx morphoGenTxId

instance Arbitrary (GenTx TestBlock) where
  arbitrary = mkMorphoGenTx <$> arbitrary

instance Arbitrary Vote where
  arbitrary = Vote <$> arbitrary <*> arbitrary

instance Arbitrary Tx where
  arbitrary = Tx <$> arbitrary

instance Arbitrary Signature where
  arbitrary = do
    sk <- arbitrary
    bytes <- generateBytes
    case sign sk bytes of
      Nothing ->
        error $
          "Invalid Generator for Signature: Couldn't sign bytes " ++ show bytes ++ " with " ++ show sk
      Just sgn -> return sgn

generateBytes :: Gen Bytes
generateBytes =
  Bytes . B.pack <$> vectorOf 32 (choose (0, 255))

instance Arbitrary PrivateKey where
  arbitrary = do
    hex <- vectorOf 64 (elements validHex)
    case bytesFromHex (T.pack hex) >>= importPrivateKey of
      Left err -> error $ "Invalid Generator for PrivateKey. Generated " <> hex <> ", which is invalid because: " <> T.unpack err
      Right sk -> pure sk
    where
      validHex = ['0' .. '9'] <> ['a' .. 'f']

instance Arbitrary PublicKey where
  arbitrary = pKey . keyPairFromPrivate <$> arbitrary

instance Arbitrary PowBlockNo where
  arbitrary = PowBlockNo <$> arbitrary

instance Arbitrary Bytes where
  arbitrary = Bytes <$> arbitrary

instance Arbitrary PowBlockRef where
  arbitrary = PowBlockRef <$> arbitrary <*> arbitrary

instance Arbitrary PowBlockHash where
  arbitrary = PowBlockHash <$> generateBytes

instance Arbitrary (HeaderHash blk) => Arbitrary (ChainHash blk) where
  arbitrary =
    oneof
      [ return GenesisHash,
        BlockHash <$> arbitrary
      ]

instance Arbitrary (SomeSecond (NestedCtxt Header) TestBlock) where
  arbitrary = return $ SomeSecond indexIsTrivial

instance Arbitrary (LedgerState TestBlock) where
  arbitrary = MorphoLedgerState <$> arbitrary

instance Arbitrary (MorphoState TestBlock) where
  arbitrary =
    MorphoState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Checkpoint where
  arbitrary = Checkpoint <$> arbitrary <*> arbitrary

instance Arbitrary (AnnTip TestBlock) where
  arbitrary = do
    annTipSlotNo <- SlotNo <$> arbitrary
    annTipBlockNo <- BlockNo <$> arbitrary
    annTipInfo <- arbitrary
    return AnnTip {..}

instance Arbitrary (TxId (GenTx TestBlock)) where
  arbitrary = MorphoGenTxId <$> arbitrary

instance Arbitrary (MorphoError TestBlock) where
  arbitrary =
    oneof
      [ MorphoWrongDistance <$> arbitrary,
        MorphoInvalidSignature <$> arbitrary,
        MorphoDuplicateVote <$> arbitrary,
        MorphoUnknownPublicKey <$> arbitrary,
        MorphoInvalidHash <$> arbitrary <*> arbitrary
      ]

instance HashAlgorithm h => Arbitrary (Hash h a) where
  arbitrary =
    UnsafeHash . SB.pack
      <$> vectorOf (fromIntegral (sizeHash (Proxy @h))) arbitrary

instance Arbitrary a => Arbitrary (WithVersion MorphoNodeToNodeVersion a) where
  arbitrary = WithVersion MorphoNodeToNodeVersion1 <$> arbitrary

instance Arbitrary a => Arbitrary (WithVersion MorphoNodeToClientVersion a) where
  arbitrary = WithVersion MorphoNodeToClientVersion1 <$> arbitrary

instance Arbitrary (HeaderHash blk) => Arbitrary (Point blk) where
  arbitrary =
    oneof
      [ return GenesisPoint,
        BlockPoint <$> arbitrary <*> arbitrary
      ]

testBftConfig :: ConsensusConfig (Bft ConsensusMockCrypto)
testBftConfig =
  BftConfig
    { bftParams -- not used to forge blocks
      =
        BftParams
          { bftSecurityParam = SecurityParam 4,
            bftNumNodes = NumCoreNodes 5
          },
      bftSignKey = SignKeyMockDSIGN 1,
      bftVerKeys = mempty -- not used to forge blocks
    }
