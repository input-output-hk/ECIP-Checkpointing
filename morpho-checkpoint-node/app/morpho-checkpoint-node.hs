import Cardano.Prelude
import Cardano.Shell.Lib
import Morpho.Common.Parsers
import Morpho.Common.TopHandler
import Morpho.Config.Types
import Morpho.Node.Env
import Morpho.Node.Run

main :: IO ()
main = toplevelExceptionHandler $ do
  (file, cliConfig) <- runCLI
  nodeConfig <- getConfiguration file cliConfig
  (env, features) <- configurationToEnv file nodeConfig
  runCardanoApplicationWithFeatures features $
    CardanoApplication $ run env
