module Gerber.Attribute.FileFunction.Index (
  Index (..),
) where


newtype Index = Index Word
  deriving (Eq, Show)
