{-# language BlockArguments #-}
module Gerber.Grammar.Tests (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.Golden (goldenVsStringDiff)
import qualified Data.Text.IO as Text
import Text.Megaparsec (errorBundlePretty)
import Data.ByteString.Lazy.Char8 (pack)
import Gerber.Grammar (parseGerber)
import Text.Show.Pretty (ppShow)

tests :: TestTree
tests =  testGroup "Gerber.Grammar.Tests"
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
  ]
  where
    mkGolden example = goldenVsStringDiff example differ ( "test-data/" ++ example ++ ".golden" ) parser
      where
        differ ref new = ["diff", "-u", ref, new]
        parser = ( parseGerber <$> Text.readFile filepath )  >>= either onFailure onSuccess
          where
            filepath = "test-data/" ++ example ++ ".gbr"
            onFailure = fail . errorBundlePretty
            onSuccess = pure . pack . ppShow

