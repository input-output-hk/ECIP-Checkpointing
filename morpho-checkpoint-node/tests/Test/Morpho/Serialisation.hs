{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Morpho.Serialisation
  ( serialiseTests,
  )
where

import Morpho.Ledger.Block
import Morpho.Ledger.Serialise
import Ouroboros.Consensus.Node.Serialisation ()
import Ouroboros.Consensus.Util (Dict (..))
import Test.Morpho.Generators
import Test.Tasty
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Serialisation
import Prelude

serialiseTests :: TestTree
serialiseTests =
  testGroup
    "Serialisation"
    [ roundtrip_all testCodecCfg dictNestedHdr
    ]
  where
    testCodecCfg :: CodecConfig TestBlock
    testCodecCfg = defaultCodecConfig
    dictNestedHdr :: forall a. NestedCtxt_ TestBlock Header a -> Dict (Eq a, Show a)
    dictNestedHdr CtxtMorpho = Dict
