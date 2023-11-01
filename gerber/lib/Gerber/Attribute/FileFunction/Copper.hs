module Gerber.Attribute.FileFunction.Copper (
  Copper (..),
  Mark (..),
  Type (..),
) where

-- gerber
import Gerber.Attribute.FileFunction.Index


data Mark = Top | Inner | Bottom
  deriving (Eq, Show)


data Copper = Copper
  { index :: !Index
  , mark :: !Mark
  , type_ :: !(Maybe Type)
  }
  deriving (Eq, Show)


data Type = Plane | Signal | Mixed | Hatched
  deriving (Eq, Show)
