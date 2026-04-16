{-# LANGUAGE BlockArguments #-}

module Gerber.Grammar.Tests (tests) where

-- bytestring
import Data.ByteString.Lazy.Char8 (pack)

-- gerber
import Gerber.Grammar (parseGerber)

-- megaparsec
import Text.Megaparsec (errorBundlePretty)

-- pretty-show
import Text.Show.Pretty (ppShow)

-- tasty
import Test.Tasty (TestTree, testGroup)

-- tasty-golden
import Test.Tasty.Golden (goldenVsStringDiff)

-- text
import qualified Data.Text.IO as Text


tests :: TestTree
tests =
  testGroup
    "Gerber.Grammar.Tests"
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
    , mkGolden "sample-1-stencil-top-panelised"
    , mkGolden "sample-2-copper-top-panelised"
    , mkGolden "sample-3-panel-outline"
    , mkGolden "stencil-top"
    ]
  where
    mkGolden example = goldenVsStringDiff example differ ("test-data/" ++ example ++ ".golden") parser
      where
        differ ref new = ["diff", "-u", ref, new]
        parser = Text.readFile filepath >>= either onFailure onSuccess . parseGerber
          where
            filepath = "test-data/" ++ example ++ ".gbr"
            onFailure = fail . errorBundlePretty
            onSuccess = pure . pack . ppShow
