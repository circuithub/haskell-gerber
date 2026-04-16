module Gerber.Padding (Padding (..)) where


data Padding
  = PadLeading
  | PadTrailing
  deriving
    (Eq, Show)
