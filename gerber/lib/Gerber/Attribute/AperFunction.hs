module Gerber.Attribute.AperFunction (
  AperFunction (..), IPC4761(..), PressFit(..), MechanicalPurpose(..), PadDefinition(..), FiducialSpecifier(..), OutlineType(..)
) where

-- gerber
import Gerber.Attribute.Attribute (Field)
-- import Gerber.Attribute.FileFunction.Copper (Copper)
-- import Gerber.Attribute.FileFunction.Drill
-- import Gerber.Attribute.FileFunction.Index
-- import Gerber.Attribute.FileFunction.Mask (Mask)
-- import Gerber.Attribute.FileFunction.Profile
-- import Gerber.Attribute.FileFunction.Side (Side)


data AperFunction
  = -- Drill and Rout layers
    ViaDrill !(Maybe IPC4761)
  | BackDrill
  | ComponentDrill !(Maybe PressFit)
  | MechanicalDrill !(Maybe MechanicalPurpose)
  | CastellatedDrill
  | OtherDrill !Field

  -- Copper Layers
  | ComponentPad
  | SMDPad !PadDefinition
  | BGAPad !PadDefinition
  | ConnectorPad
  | HeatsinkPad
  | ViaPad
  | TestPad
  | CastellatedPad
  | FiducialPad !FiducialSpecifier
  | ThermalReliefPad
  | WasherPad
  | AntiPad
  | OtherPad !Field
  | Conductor
  | EtchedComponent
  | NonConductor
  | CopperBalancing
  | Border
  | OtherCopper !Field

  -- Component Layers
  | ComponentMain
  | ComponentOutline !OutlineType
  | ComponentPin

  -- All data layers
  | Profile
  | NonMaterial
  | Material
  | Other !Field
  deriving (Eq, Show)


data IPC4761
  = Ia
  | Ib
  | IIa
  | IIb
  | IIIa
  | IIIb
  | IVa
  | IVb
  | V
  | VI
  | VII
  | None
  deriving (Eq, Show)


data PressFit = PressFit deriving (Eq, Show)

data MechanicalPurpose = Tooling | Breakout | OtherPurpose deriving (Eq, Show)

data PadDefinition = CuDef | SMDef deriving (Eq, Show)

data FiducialSpecifier = Local | Global | Panel deriving (Eq, Show)

data OutlineType = Body | Lead2Lead | Footprint | Courtyard deriving (Eq, Show)
