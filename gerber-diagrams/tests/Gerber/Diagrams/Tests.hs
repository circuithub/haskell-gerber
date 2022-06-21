{-# language BlockArguments #-}
module Gerber.Diagrams.Tests (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Golden ( goldenVsFileDiff )
import qualified Data.Text.IO as Text
import Text.Megaparsec ( errorBundlePretty )
import Gerber.Grammar ( parseGerber )
import Gerber.Diagrams ( gerberToDiagram )
import qualified Diagrams.Backend.Cairo
import qualified Diagrams.Prelude as Diagrams
import qualified Control.Foldl as L
import Gerber.Evaluate ( evaluate )

tests :: TestTree
tests =  testGroup "Gerber.Diagram.Tests"
  [ mkGolden "2-13-1_Two_square_boxes"
  , mkGolden "2-13-2_Polarities_and_Apertures"
  , mkGolden "4-11-6_Block_with_different_orientations"
  , mkGolden "4-6-4_Nested_blocks"
  , mkGolden "6-1-6-2_A_drill_file"
  , mkGolden "sample_macro"
  , mkGolden "sample_macro_X1"
  , mkGolden "SMD_prim_20"
  , mkGolden "SMD_prim_20_X1"
  , mkGolden "SMD_prim_21"
  , mkGolden "SMD_prim_21_X1"
  , mkGolden "moire"
  , mkGolden "thermal"
  , mkGolden "sample-1-stencil-top"
  , mkGolden "sample-1-stencil-top-panelised"
  , mkGolden "sample-2-copper-top-panelised"
  , mkGolden "haskell-badge-copper-bottom"
  , mkGolden "haskell-badge-copper-top"
  , mkGolden "haskell-badge-cutouts"
  , mkGolden "haskell-badge-keepout"
  , mkGolden "haskell-badge-mechanical-1"
  , mkGolden "haskell-badge-mechanical-13"
  , mkGolden "haskell-badge-mechanical-2"
  , mkGolden "haskell-badge-mechanical-3"
  , mkGolden "haskell-badge-mechanical-4"
  , mkGolden "haskell-badge-outline"
  , mkGolden "haskell-badge-silkscreen-bottom"
  , mkGolden "haskell-badge-silkscreen-top"
  , mkGolden "haskell-badge-solder-mask-bottom"
  , mkGolden "haskell-badge-solder-mask-top"
  , mkGolden "haskell-badge-stencil-bottom"
  , mkGolden "haskell-badge-stencil-top"
  ]
  where
    mkGolden example = goldenVsFileDiff example differ goldenPath testPath writeOutput
      where
        goldenPath = "test-data/" ++ example ++ ".golden.png"
        testPath = "test-data/" ++ example ++ ".test.png"
        differ ref new = ["diff", ref, new]
        writeOutput = Text.readFile filepath  >>= either onFailure onSuccess . parseGerber
          where
            filepath = "test-data/" ++ example ++ ".gbr"
            onFailure = fail . errorBundlePretty
            onSuccess = Diagrams.Backend.Cairo.renderCairo testPath ( Diagrams.mkWidth 1200 ) . drawDiagram
              where
                drawDiagram = Diagrams.bg Diagrams.white . Diagrams.frame 1.1 . L.fold ( evaluate gerberToDiagram )

