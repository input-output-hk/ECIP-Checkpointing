import Cardano.Prelude
import Morpho.Common.Parsers
import Morpho.Common.TopHandler
import Morpho.Config.Combined
import Morpho.Node.Env
import Morpho.Node.Run

main :: IO ()
main = toplevelExceptionHandler $ do
  (file, cliConfig) <- runCLI
  nodeConfig <- getConfiguration cliConfig file
  withEnv nodeConfig run
