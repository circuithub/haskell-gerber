module Gerber.ApertureDefinition where


data ApertureDefinition
  = Circle !CircleModifiers
  | Rectangle !RectangleModifiers
  | Obround !RectangleModifiers
  | Polygon !PolygonModifiers
  deriving (Eq, Ord, Show)


data CircleModifiers = CircleModifiers
  { diameter :: !Double
  , circleHoleDiameter :: !(Maybe Double)
  }
  deriving (Eq, Ord, Show)


data RectangleModifiers = RectangleModifiers
  { width :: !Double
  , height :: !Double
  , rectangleHoleDiameter :: !(Maybe Double)
  }
  deriving (Eq, Ord, Show)


data PolygonModifiers = PolygonModifiers
  { outerDiameter :: !Double
  , numberOfVertices :: !Int
  , rotation :: !(Maybe Double)
  , polygonHoleDiameter :: !(Maybe Double)
  }
  deriving (Eq, Ord, Show)
