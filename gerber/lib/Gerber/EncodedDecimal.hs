module Gerber.EncodedDecimal ( EncodedDecimal(..) ) where

data EncodedDecimal =
  EncodedDecimal
    { negative :: Bool
    , digits :: [ Int ]
    }
  deriving
    ( Eq, Show )
