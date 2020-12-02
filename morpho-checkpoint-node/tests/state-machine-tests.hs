import Test.Morpho.QSM
import Test.Tasty
import Prelude

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
  return $
    testGroup
      "State Machine Tests"
      [ qsmTests
      ]
