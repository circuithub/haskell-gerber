module Gerber.Format where


data Format = Format
  { integerPositions :: !Int
  , decimalPositions :: !Int
  }
  deriving (Eq, Show)
