module Gerber.Attribute.Part (
  Part (..),
) where

-- gerber
import Gerber.Attribute.Attribute (Field)


data Part
  = Single
  | Array
  | FabricationPanel
  | Coupon
  | Other !Field
  deriving (Eq, Show)
