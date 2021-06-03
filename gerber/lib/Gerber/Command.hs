module Gerber.Command where

import qualified Data.Text as StrictText

import Gerber.ApertureDefinition ( ApertureDefinition )
import Gerber.MacroDefinition ( Definition, Modifier )
import Gerber.DCodeNumber ( DCodeNumber )
import Gerber.Format ( Format )
import Gerber.Mirroring ( Mirroring )
import Gerber.Movement ( Movement )
import Gerber.Padding ( Padding )
import Gerber.Polarity ( Polarity )
import Gerber.StepRepeat ( StepRepeat )
import Gerber.Unit ( Unit )


data Command
  = FS !Padding !Format !Format
  | AD !DCodeNumber !ApertureDefinition
  | MacroAD !DCodeNumber !StrictText.Text ![Float]
  | D !DCodeNumber
  | D01 !Movement
  | D02 !Movement
  | D03 !Movement
  | G01
  | G02
  | G03
  | G04 !StrictText.Text
  | G36
  | G37
  | G74
  | G75
  | G71
  | IP
  | LP !Polarity
  | MO !Unit
  | OF !( Maybe Float ) !( Maybe Float )
  | AM !StrictText.Text ![Definition Modifier Modifier]
  | SR !StepRepeat
  | SR_End
  | M02
  | SF
  | MI
  | AB !DCodeNumber
  | AB_End
  | LM !Mirroring
  | LR !Float
  | LS !Float
  | IngoredAttributeTF !StrictText.Text
  | IngoredAttributeTA !StrictText.Text
  | IngoredAttributeTO !StrictText.Text
  | IngoredAttributeTD !StrictText.Text
  deriving ( Eq, Show )
