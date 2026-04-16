module Gerber.Attribute.GenerationSoftware (
  GenerationSoftware (..),
) where

-- gerber
import Gerber.Attribute.Attribute (Field)


data GenerationSoftware = GenerationSoftware
  { vendor :: !Field
  , application :: !Field
  , version :: !(Maybe Field)
  }
  deriving (Eq, Show)
