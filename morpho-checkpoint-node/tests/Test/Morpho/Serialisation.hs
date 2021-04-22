{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.Serialisation
  ( serialiseTests,
  )
where

import Cardano.Prelude
import Morpho.Ledger.Block
import Morpho.Ledger.Serialise
import Ouroboros.Consensus.Util (Dict (..))
import Test.Morpho.Generators
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Serialisation.Roundtrip

serialiseTests :: TestTree
serialiseTests =
  testGroup
    "Serialisation"
    [ testGroup "SerialiseDisk" $ roundtrip_SerialiseDisk testCodecCfg dictNestedHdr,
      testGroup "SerialiseNodeToNode" $ roundtrip_SerialiseNodeToNode testCodecCfg,
      -- We don't have any node-to-client queries, so we can't derive an Arbitrary instance for testing it
      --, testGroup "SerialiseNodeToClient" $ roundtrip_SerialiseNodeToClient testCodecCfg
      testProperty "envelopes" $ roundtrip_envelopes testCodecCfg,
      testProperty "ConvertRawHash" $ roundtrip_ConvertRawHash (Proxy @TestBlock),
      testProperty "hashSize" $ prop_hashSize (Proxy @TestBlock)
      -- Currently the prop_estimateBlockSize is not exported by Test.Util.Serialisation.Roundtrip
      -- TODO: Uncomment after https://github.com/input-output-hk/ouroboros-network/pull/2972 is included
      --, testProperty "estimateBlockSize"  $ prop_estimateBlockSize         testCodecCfg
    ]
  where
    testCodecCfg :: CodecConfig TestBlock
    testCodecCfg = MorphoCodecConfig ()
    dictNestedHdr :: forall a. NestedCtxt_ TestBlock Header a -> Dict (Eq a, Show a)
    dictNestedHdr CtxtMorpho = Dict
