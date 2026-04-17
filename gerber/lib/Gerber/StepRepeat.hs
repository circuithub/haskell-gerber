module Gerber.StepRepeat (StepRepeat (..)) where


data StepRepeat = StepRepeat
  { xRepeats :: !Int
  , yRepeats :: !Int
  , xStep :: !Double
  , yStep :: !Double
  }
  deriving (Eq, Show)
