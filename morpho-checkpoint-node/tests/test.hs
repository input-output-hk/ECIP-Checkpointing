import Test.Morpho.Common.Utils
import Test.Morpho.Crypto.ECDSASignature
import Test.Morpho.Golden
import Test.Morpho.Ledger.State
import Test.Morpho.QSM
import Test.Morpho.Serialisation
import Test.Tasty
import Prelude

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
  utilsTests <- utilsTestsIO
  return $
    testGroup
      "Morpho"
      [ stateTests,
        ecdsaTests,
        utilsTests,
        serialiseTests,
        goldenTests,
        qsmTests
      ]
