module Gerber.Attribute (
  FileAttribute (..),
) where

-- gerber
import Gerber.Attribute.Attribute
import Gerber.Attribute.CreationDate
import Gerber.Attribute.FileFunction
import Gerber.Attribute.FilePolarity
import Gerber.Attribute.GenerationSoftware
import Gerber.Attribute.MD5
import Gerber.Attribute.Part
import Gerber.Attribute.ProjectId

-- text
import Data.Text (Text)


data FileAttribute
  = Part !Part
  | FileFunction !FileFunction
  | FilePolarity !FilePolarity
  | GenerationSoftware !GenerationSoftware
  | SameCoordinates !(Maybe Text)
  | CreationDate !CreationDate
  | ProjectId !ProjectId
  | MD5 !MD5
  deriving (Eq, Show)
