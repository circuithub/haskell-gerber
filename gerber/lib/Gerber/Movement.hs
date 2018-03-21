module Gerber.Movement ( Movement(..) ) where

data Movement = Movement
  { x :: !( Maybe Int )
  , y :: !( Maybe Int )
  , i :: !( Maybe Int )
  , j :: !( Maybe Int )
  }
  deriving ( Eq, Show )
