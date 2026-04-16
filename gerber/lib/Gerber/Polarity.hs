module Gerber.Polarity (Polarity (..)) where


data Polarity = Clear | Dark
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
