module Gerber.Attribute.Attribute (
  Attribute (..),
  Field,
) where

-- text
import Data.Text (Text)


type Field = Text


data Attribute = Attribute
  { name :: !Text
  , value :: ![Field]
  }
  deriving (Eq, Show)
