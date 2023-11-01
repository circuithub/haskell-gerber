module Gerber.Attribute.FileFunction (
  FileFunction (..),
) where

-- gerber
import Gerber.Attribute.Attribute (Field)
import Gerber.Attribute.FileFunction.Copper (Copper)
import Gerber.Attribute.FileFunction.Drill
import Gerber.Attribute.FileFunction.Index
import Gerber.Attribute.FileFunction.Mask (Mask)
import Gerber.Attribute.FileFunction.Profile
import Gerber.Attribute.FileFunction.Side (Side)


data FileFunction
  = Copper !Copper
  | Soldermask !Mask
  | Legend !Mask
  | Goldmask !Mask
  | Silvermask !Mask
  | Tinmask !Mask
  | Carbonmask !Mask
  | Peelablemask !Mask
  | Glue !Side
  | Viatenting !Side
  | Viafill
  | Heatsink !Side
  | Heatsinkmask !Mask
  | Paste !Side
  | KeepOut !Side
  | Pads !Side
  | Scoring !Side
  | Plated !Drill
  | NonPlated !Drill
  | Profile !Profile
  | Drillmap
  | FabricationDrawing
  | ArrayDrawing
  | AssemblyDrawing !Side
  | Drawing !Field
  | Other !Field
  | OtherDrawing !Field
  | Component !Index !Side
  | Depthrout !Side
  | Vcut !(Maybe Side)
  | Vcutmap
  deriving (Eq, Show)
