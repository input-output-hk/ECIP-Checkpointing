import Test.Morpho.MantisIntegration
import Test.Tasty
import Prelude

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
  return $
    testGroup
      "Mantis Integration Tests"
      [ mantisIntegrationTests
      ]
