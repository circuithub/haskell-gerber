module Gerber.Attribute.FileFunction.Mask (
  Mask (..),
  Index (..),
) where

-- gerber
import Gerber.Attribute.FileFunction.Index
import Gerber.Attribute.FileFunction.Side (Side)


data Mask = Mask
  { side :: !Side
  , index :: !(Maybe Index)
  }
  deriving (Eq, Show)
