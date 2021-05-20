module Gerber.ApertureDefinition where


data ApertureDefinition
  = Circle !CircleModifiers
  | Rectangle !RectangleModifiers
  | Obround !RectangleModifiers
  | Polygon !PolygonModifiers
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


data PolygonModifiers = PolygonModifiers
  { outerDiameter :: !Float
  , numberOfVertices :: !Int
  , rotation :: !( Maybe Float )
  , polygonHoleDiameter :: !( Maybe Float )
  }
  deriving ( Eq, Show )
