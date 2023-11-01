module Gerber.StepRepeat (StepRepeat (..)) where


data StepRepeat = StepRepeat
  { xRepeats :: !Int
  , yRepeats :: !Int
  , xStep :: !Float
  , yStep :: !Float
  }
  deriving (Eq, Show)
