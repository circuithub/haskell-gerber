module Gerber.Evaluate.Edge where


data Edge
  = Line (Double, Double)
  | ArcCW (Double, Double) (Double, Double)
  | ArcCCW (Double, Double) (Double, Double)
  deriving (Show)
