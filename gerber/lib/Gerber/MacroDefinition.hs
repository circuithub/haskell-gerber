{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
module Gerber.MacroDefinition where

import Data.Text (Text)
import Linear.V2 (V2)
import Data.List.NonEmpty (NonEmpty)

data Exposure
  = ExposureOn
  | ExposureOff
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Modifier
  = Decimal Float
  | VariableReference Int
  | Parentheses Modifier
  | UnaryPlus Modifier
  | UnaryMinus Modifier
  | Plus Modifier Modifier
  | Minus Modifier Modifier
  | Multiply Modifier Modifier
  | Divide Modifier Modifier
  deriving (Show, Read, Eq, Ord)

data Definition exposure a
  = Variable !Int !a
  | Comment !Text
  | Primitive !(Primitive exposure a)
  | InvalidDefinition !Int Text
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data Primitive exposure a
  = Circle !(CircleModifiers exposure a)
  | VectorLine !(VectorLineModifiers exposure a)
  | CenterLine !(CenterLineModifiers exposure a)
  | Outline !(OutlineModifiers exposure a)
  | Polygon !(PolygonModifiers exposure a)
  | Moire !(MoireModifiers a)
  | Thermal !(ThermalModifiers a)
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data CircleModifiers exposure a = CircleModifiers
  { circleExposure :: exposure
  , diameter :: !a
  , circleCenter :: !(V2 a)
  , circleRotation :: !(Maybe a)
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data VectorLineModifiers exposure a = VectorLineModifiers
  { vectorLineExposure :: exposure
  , vectorLineWidth :: !a
  , start :: !(V2 a)
  , end :: !(V2 a)
  , vectorLineRotation :: !a
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data CenterLineModifiers exposure a = CenterLineModifiers
  { centerLineExposure :: exposure
  , centerLineWidth :: !a
  , height :: !a
  , centerLineCenter :: !(V2 a)
  , centerLineRotation :: !a
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data OutlineModifiers exposure a = OutlineModifiers
  { outlineExposure :: exposure
  , vertices :: !(NonEmpty (V2 a))
  , outlineRotation :: !a
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)


data PolygonModifiers exposure a = PolygonModifiers
  { polygonExposure :: exposure
  , numVertices :: !Int
  , polygonCenter :: !(V2 a)
  , polygonDiameter :: !a
  , polygonRotation :: !a
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data MoireModifiers a = MoireModifiers
  { moireCenter :: !(V2 a)
  , outerRingDiameter :: !a
  , ringThickness :: !a
  , ringGap :: !a
  , maximumNumberOfRings :: !a
  , crossThickness :: !a
  , crossLength :: !a
  , moireRotation :: !a
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data ThermalModifiers a = ThermalModifiers
  { thermalCenter :: !(V2 a)
  , outerDiameter :: !a
  , innerDiameter :: !a
  , gapThickness :: !a
  , thermalRotation :: !a
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)
