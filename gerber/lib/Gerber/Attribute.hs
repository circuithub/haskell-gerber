module Gerber.Attribute (
  FileAttribute (..),
  ApertureAttribute (..),
  Tolerance (..),
) where

-- gerber
import Gerber.Attribute.AperFunction
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


data ApertureAttribute
  = AperFunction !AperFunction
  | DrillTolerance !Tolerance
  | FlashText
      { text :: Text
      , bOrC :: Char
      , rOrM :: Maybe Char
      , font :: Maybe Text
      , fontSize :: Maybe Int
      , comment :: Maybe Text
      }
  deriving (Eq, Show)


data Tolerance = Tolerance
  { plusTolerance :: !Float
  , minusTolerance :: !Float
  }
  deriving (Eq, Show)
