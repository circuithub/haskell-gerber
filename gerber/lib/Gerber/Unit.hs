module Gerber.Unit where

data Unit = MM | IN
  deriving ( Bounded, Enum, Eq, Ord, Read, Show )
