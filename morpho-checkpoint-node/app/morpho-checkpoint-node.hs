import Cardano.Prelude
import Morpho.Common.Parsers
import Morpho.Common.TopHandler
import Morpho.Config.Types
import Morpho.Node.Features.Node
import qualified Options.Applicative as Opt
import Options.Applicative.Help ((<$$>))
import qualified Options.Applicative.Help as OptI
import Prelude (String)

main :: IO ()
main = toplevelExceptionHandler $ do
  cli <- Opt.execParser opts
  run cli
  where
    opts :: Opt.ParserInfo NodeCLI
    opts =
      Opt.info
        ( nodeCliParser
            <**> helperBrief "help" "Show this help text" nodeCliHelpMain
        )
        ( Opt.fullDesc
            <> Opt.progDesc "Start a OBFT-Checkpoint node."
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
