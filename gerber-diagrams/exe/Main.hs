{-# language ApplicativeDo #-}
{-# language RecordWildCards #-}

module Main where

-- diagrams
import qualified Diagrams.Prelude as Diagrams

-- diagrams-cairo
import qualified Diagrams.Backend.Cairo

-- foldl
import qualified Control.Foldl as L

-- gerber
import qualified Gerber.Evaluate as Gerber
import qualified Gerber.Grammar as Gerber

-- gerber-diagrams
import Gerber.Diagrams ( gerberToDiagram )

-- optparse-applicative
import qualified Options.Applicative as OptParse

-- rio
import RIO

-- text
import qualified Data.Text.IO as StrictText


data CLIOptions = CLIOptions
  { gerberFilePaths :: ![ FilePath ]
  , scale :: !Double
  , outputPath :: !FilePath
  }


cliOptionsParser :: OptParse.Parser CLIOptions
cliOptionsParser = do
  scale <-
    OptParse.option
      OptParse.auto
      ( mconcat
          [ OptParse.long "scale"
          , OptParse.help "The scale of pixels-per-mm"
          , OptParse.value 10
          , OptParse.showDefault
          ]
      )

  outputPath <-
    OptParse.strArgument
      ( mconcat
          [ OptParse.metavar "OUTPUT"
          , OptParse.help "The path to the rendered PNG"
          ]
      )


  gerberFilePaths <-
    some
      ( OptParse.strArgument
          ( mconcat
              [ OptParse.metavar "GERBER"
              , OptParse.help "The path to a Gerber file"
              ]
          )
      )

  return CLIOptions{..}


mainWith :: CLIOptions -> IO ()
mainWith CLIOptions{..} = do
  gerbers <-
    fmap
      catMaybes
      ( traverse
          ( \path -> do
              source <-
                tryAny ( StrictText.readFile path )

              case source of
                Left e -> do
                  putStrLn ( "Could not parse '" <> path <> "'" )
                  return Nothing

                Right source ->
                  case Gerber.parseGerber source of
                    Left e -> do
                      putStrLn ( "Could not parse '" <> path <> "':" )
                      putStrLn ( displayException e )
                      return Nothing

                    Right ok ->
                      return ( Just ok )
          )
          gerberFilePaths
      )

  let
    diagrams =
      map ( L.fold ( Gerber.evaluate gerberToDiagram ) ) gerbers

    d =
      mconcat
        ( zipWith
            ( \col d -> Diagrams.lc col ( Diagrams.fc col d ) )
            ( cycle [ Diagrams.red, Diagrams.green, Diagrams.blue ] )
            diagrams
        )

  Diagrams.Backend.Cairo.renderCairo
    outputPath
    ( Diagrams.mkWidth ( scale * Diagrams.width d ) )
    ( Diagrams.bg Diagrams.white ( Diagrams.pad 1.1 d ) )


main :: IO ()
main = do
  cliOptions <-
    OptParse.execParser ( OptParse.info cliOptionsParser mempty )

  mainWith cliOptions
