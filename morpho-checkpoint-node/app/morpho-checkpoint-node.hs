import Cardano.Prelude
import Cardano.Shell.Lib (CardanoApplication (..), runCardanoApplicationWithFeatures)
import Cardano.Shell.Types (CardanoFeature (..))
import Morpho.Common.Parsers
import Morpho.Common.TopHandler
import Morpho.Config.Logging (createLoggingFeature)
import Morpho.Config.Types
import Morpho.Node.Features.Node
import qualified Options.Applicative as Opt
import Options.Applicative.Help ((<$$>))
import qualified Options.Applicative.Help as OptI
import Prelude (String)

main :: IO ()
main = toplevelExceptionHandler $ do
  cli <- Opt.execParser opts
  (features, nodeLayer) <- initializeAllFeatures cli
  runCardanoApplicationWithFeatures features (cardanoApplication nodeLayer)
  where
    opts :: Opt.ParserInfo NodeCLI
    opts =
      Opt.info
        ( nodeCliParser
            <**> helperBrief "help" "Show this help text" nodeCliHelpMain
            <**> helperBrief "help-tracing" "Show help for tracing options" cliHelpTracing
        )
        ( Opt.fullDesc
            <> Opt.progDesc "Start node of the Cardano blockchain."
        )
    helperBrief :: String -> String -> String -> Opt.Parser (a -> a)
    helperBrief l d helpText =
      Opt.abortOption (Opt.InfoMsg helpText) $
        mconcat
          [ Opt.long l,
            Opt.help d
          ]
    nodeCliHelpMain :: String
    nodeCliHelpMain =
      renderHelpDoc 80 $
        parserHelpHeader "morpho-checkpoint-node" nodeCliParser
          <$$> ""
          <$$> parserHelpOptions nodeCliParser
    cliHelpTracing :: String
    cliHelpTracing =
      renderHelpDoc 80 $
        "Additional tracing options:"
          <$$> ""
          <$$> parserHelpOptions cliTracingParser
    cardanoApplication :: NodeLayer -> CardanoApplication
    cardanoApplication = CardanoApplication . nlRunNode

initializeAllFeatures ::
  NodeCLI ->
  IO ([CardanoFeature], NodeLayer)
initializeAllFeatures nCli@NodeCLI {configFp = ncFp} = do
  (loggingLayer, loggingFeature) <- createLoggingFeature nCli
  nodeConfig <- parseNodeConfiguration $ unConfigPath ncFp
  (nodeLayer, nodeFeature) <- createNodeFeature loggingLayer nodeConfig nCli
  pure
    ( [loggingFeature, nodeFeature] :: [CardanoFeature],
      nodeLayer
    )

-- | Produce just the brief help header for a given CLI option parser,
--   without the options.
parserHelpHeader :: String -> Opt.Parser a -> OptI.Doc
parserHelpHeader execName = flip (OptI.parserUsage (Opt.prefs mempty)) execName

-- | Produce just the options help for a given CLI option parser,
--   without the header.
parserHelpOptions :: Opt.Parser a -> OptI.Doc
parserHelpOptions = fromMaybe mempty . OptI.unChunk . OptI.fullDesc (Opt.prefs mempty)

-- | Render the help pretty document.
renderHelpDoc :: Int -> OptI.Doc -> String
renderHelpDoc cols =
  (`OptI.displayS` "") . OptI.renderPretty 1.0 cols
