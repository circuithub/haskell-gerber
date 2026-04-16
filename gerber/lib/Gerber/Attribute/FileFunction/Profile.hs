module Gerber.Attribute.FileFunction.Profile (
  Profile (..),
) where


data Profile = Plated | NonPlated
  deriving (Eq, Show)
