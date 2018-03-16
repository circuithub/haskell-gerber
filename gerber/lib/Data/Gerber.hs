{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:      Data.Gerber
--
-- Work with gerber files
module Data.Gerber
  ( module Data.Gerber.Types
  , ParseError (..)
  , parseGerberFile
  , strokeAperatures
  , strokeAperatures'
  , minStrokeWidthMM
  , minStrokeWidthMM'
  )
  where

import Control.Arrow
import Control.Monad.Trans.Reader
import qualified Data.Gerber.Parser as Parser
import Data.Gerber.Types
import Data.List (foldl')
import Data.Map ( Map )
import Data.Functor.Identity ( runIdentity )

import Data.Text (Text)
import Text.Megaparsec
import Data.Functor.Foldable
import qualified Data.Map as Map
import Data.Monoid (First (..))
import qualified Data.List.NonEmpty as NE
import Data.Foldable (minimum)

-- | Parse a gerber file returning an error message on failure
parseGerberFile filename b =
  runIdentity
     (runParserT
        (runReaderT Parser.gerberFile Parser.initialContext)
        filename
        b)

-- | Extract the minimum stroke width from a gerber file in millimetres
minStrokeWidthMM :: GerberFile -> Maybe Double
minStrokeWidthMM = minStrokeWidthMM' . toGerberFileF

-- | Extract the minimum stroke width from a gerber file in millimetres
minStrokeWidthMM' :: Fix GerberCommandF -> Maybe Double
minStrokeWidthMM' g = do
  units <- fst unitsStrokes
  strokes <- NE.nonEmpty . map toWidth . snd $ unitsStrokes
  pure (toMm units (minimum strokes))
  where
    unitsStrokes = strokeAperatures' g
    toWidth (_, CircleStrokeAd r) = r
    toWidth (_, RectangleStrokeAd {..}) = min rectangleStrokeAdX rectangleStrokeAdY
    toMm Milimeters a = a
    toMm Inches a = a * 25.4

-- | Get all the strokes used in the gerber file (interpolate to which aren't in a region)
strokeAperatures :: GerberFile -> (Maybe Units, [(AperaturId, StrokeAd)])
strokeAperatures = strokeAperatures' . toGerberFileF

-- | Get all the strokes used in the gerber file (interpolate to which aren't in a region)
strokeAperatures' :: Fix GerberCommandF -> (Maybe Units, [(AperaturId, StrokeAd)])
strokeAperatures' =
  second noNulls .
  second byAd . second (second bySetApp) . first getFirst . collectGerberFileF malg
  where
    malg
      :: GerberCommandF (First Units, (Map AperaturId StrokeAd, [Either SetAperature InterpolateTo]))
      -> Maybe (First Units, (Map AperaturId StrokeAd, [Either SetAperature InterpolateTo]))
    malg (AperatureDefF a) =
      case aperatureDefType a of
        StrokeAdt s -> Just (First Nothing, (Map.singleton (aperatureDefId a) s, []))
        StandardAdt _ -> Just mempty
        MacroAdt _ -> Just mempty
    malg (UnitsF u) = Just (First (Just u), (Map.empty, []))
    malg (InterpolateToF o) = Just (First Nothing, (Map.empty, [Right o]))
    malg (RegionF _) = Just mempty
    malg (SetAperatureF x) = Just (First Nothing, (Map.empty, [Left x]))
    malg _ = Nothing
    ------
    bySetApp
      :: [Either SetAperature InterpolateTo]
      -> [(AperaturId, [InterpolateTo] -> [InterpolateTo])]
    bySetApp = foldl' cmb []
      where
        cmb xs (Left (SetAperature sid)) = (sid, id) : xs
        cmb [] (Right _) = []
        cmb ((sid, os):xs) (Right o) = (sid, os . (o :)) : xs
    ------
    byAd
      :: (Map AperaturId StrokeAd, [(AperaturId, [InterpolateTo] -> [InterpolateTo])])
      -> Map AperaturId (StrokeAd, [InterpolateTo])
    byAd (m, as) = second ($ []) <$> foldl' cmb (fmap (id &&& const id) m) as
      where
        cmb m' (adid, os) = Map.update (Just . second (. os)) adid m'
    ---
    noNulls :: Map AperaturId (StrokeAd, [InterpolateTo]) -> [(AperaturId, StrokeAd)]
    noNulls = map (second fst) . filter (not . null . snd . snd) . Map.toList

