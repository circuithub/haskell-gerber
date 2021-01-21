module Gerber.Attribute.Attribute
  ( Attribute(..)
  , Field
  ) where

-- text
import Data.Text ( Text )


data Attribute = Attribute
  { name :: !Text
  , value :: ![Field]
  }
  deriving ( Eq, Show )


type Field = Text
