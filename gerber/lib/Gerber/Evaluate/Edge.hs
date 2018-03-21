module Gerber.Evaluate.Edge where

data Edge
  = Line ( Float, Float )
  | ArcCW ( Float, Float ) ( Float, Float )
  | ArcCCW ( Float, Float ) ( Float, Float )
  deriving ( Show )
