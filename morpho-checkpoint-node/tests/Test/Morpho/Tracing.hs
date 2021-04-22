module Test.Morpho.Tracing (traceTests) where

import Cardano.Prelude
import Data.Aeson
import Morpho.Tracing.Verbosity
import Test.Morpho.Generators ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

traceTests :: TestTree
traceTests =
  testGroup
    "Tracing"
    [ testGroup
        "LimitRecursion"
        [ testProperty "IfLimited" returnsIfLimited,
          testProperty "LimitDepth" limitDepth
        ]
    ]

returnsIfLimited :: Property
returnsIfLimited = property $ \limit value ->
  let (Any didLimit, limited) = limitRecursion limit value
   in didLimit /= (value == limited)

valueDepth :: Value -> Int
valueDepth (Object xs) = 1 + maximum (map valueDepth xs)
valueDepth (Array xs) = 0 + maximum (map valueDepth xs)
valueDepth _ = 0

limitDepth :: Property
limitDepth = forAll (suchThat arbitrary (>= 0)) $ \limit ->
  property $ \value -> valueDepth (snd $ limitRecursion limit value) <= limit
