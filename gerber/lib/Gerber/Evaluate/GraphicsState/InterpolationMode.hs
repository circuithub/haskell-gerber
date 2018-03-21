module Gerber.Evaluate.GraphicsState.InterpolationMode where

data InterpolationMode
  = Linear
  | CircularCCW
  | CircularCW
  deriving ( Eq, Ord, Read, Show, Enum, Bounded )
