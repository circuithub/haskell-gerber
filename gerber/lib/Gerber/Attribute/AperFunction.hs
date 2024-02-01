module Gerber.Attribute.AperFunction (
  AperFunction (..),
  IPC4761 (..),
  PressFit (..),
  MechanicalPurpose (..),
  PadDefinition (..),
  FiducialSpecifier (..),
  OutlineType (..),
  IPC4761Deprecated (..),
) where

-- gerber
import Gerber.Attribute.Attribute (Field)


data AperFunction
  = -- Drill and Rout layers
    ViaDrill !(Maybe IPC4761)
  | BackDrill
  | ComponentDrill !(Maybe PressFit)
  | MechanicalDrill !(Maybe MechanicalPurpose)
  | CastellatedDrill
  | OtherDrill !Field
  | -- Copper Layers
    ComponentPad
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
  | -- Component Layers
    ComponentMain
  | ComponentOutline !OutlineType
  | ComponentPin
  | -- All data layers
    Profile
  | NonMaterial
  | Material
  | Other !Field
  | -- Deprecated
    Drawing
  | CutOut
  | Slot
  | Cavity
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
  | Deprecated IPC4761Deprecated
  deriving (Eq, Show)


{- These options were deprecated in revision 2019.09 of the Gerber X2 spec -}
data IPC4761Deprecated
  = Filled
  | NotFilled
  deriving (Eq, Show)


data PressFit = PressFit deriving (Eq, Show)


data MechanicalPurpose = Tooling | Breakout | OtherPurpose deriving (Eq, Show)


data PadDefinition = CuDef | SMDef deriving (Eq, Show)


data FiducialSpecifier = Local | Global | Panel deriving (Eq, Show)


data OutlineType = Body | Lead2Lead | Footprint | Courtyard deriving (Eq, Show)
