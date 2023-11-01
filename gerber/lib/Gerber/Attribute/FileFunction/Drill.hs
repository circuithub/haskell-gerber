module Gerber.Attribute.FileFunction.Drill (
  Drill (..),
  Type (..),
  Via (..),
) where

-- gerber
import Gerber.Attribute.FileFunction.Index


data Drill = Drill
  { from :: !Index
  , to :: !Index
  , via :: !Via
  , type_ :: !(Maybe Type)
  }
  deriving (Eq, Show)


data Type = DrillHole | Route | Mixed
  deriving (Eq, Show)


data Via = PTH | NPTH | Blind | Buried
  deriving (Eq, Show)
