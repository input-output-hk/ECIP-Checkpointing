import Cardano.Prelude
import Cardano.Shell.Lib
import Morpho.Common.Parsers
import Morpho.Common.TopHandler
import Morpho.Config.Logging
import Morpho.Config.Types
import Morpho.Node.Env
import Morpho.Node.Run

main :: IO ()
main = toplevelExceptionHandler $ do
  (file, cliConfig) <- runCLI
  nodeConfig <- getConfiguration file cliConfig
  (loggingLayer, logging) <- loggingFeatures file (runIdentity $ ncLoggingSwitch nodeConfig)
  env <- configurationToEnv loggingLayer nodeConfig
  runCardanoApplicationWithFeatures logging $
    CardanoApplication $ run env
