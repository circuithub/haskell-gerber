module Gerber.Evaluate.GraphicsState.ApertureEntry (
  ApertureEntry (..),
)
where

-- gerber
import qualified Gerber.ApertureDefinition as Gerber
import qualified Gerber.Command as Gerber
import qualified Gerber.MacroDefinition as MacroDefinition


data ApertureEntry
  = BasicAperture !Gerber.ApertureDefinition
  | MacroAperture ![MacroDefinition.Primitive MacroDefinition.Exposure Float]
  | BlockAperture ![Gerber.Command]
