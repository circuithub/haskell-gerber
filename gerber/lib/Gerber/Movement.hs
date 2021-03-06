module Gerber.Movement ( Movement(..) ) where

import Gerber.EncodedDecimal ( EncodedDecimal )

data Movement = Movement
  { x :: !( Maybe EncodedDecimal )
  , y :: !( Maybe EncodedDecimal )
  , i :: !( Maybe EncodedDecimal )
  , j :: !( Maybe EncodedDecimal )
  }
  deriving ( Eq, Show )
