module Gerber.Flash where

data Flash = Flash
  { x :: !(Maybe Int)
  , y :: !(Maybe Int) 
  }
  deriving ( Eq, Show )
