module Gerber.ApertureDefinition where

import qualified Data.Text as StrictText


data ApertureDefinition
  = Circle !CircleModifiers
  | Rectangle !RectangleModifiers
  | Obround !RectangleModifiers
  | Macro !StrictText.Text
  deriving ( Eq, Show )


data CircleModifiers = CircleModifiers
  { diameter :: !Float
  , circleHoleDiameter :: !( Maybe Float )
  }
  deriving ( Eq, Show )


data RectangleModifiers = RectangleModifiers
  { width :: !Float
  , height :: !Float
  , rectangleHoleDiameter :: !( Maybe Float )
  }
  deriving ( Eq, Show )
