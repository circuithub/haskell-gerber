module Gerber.Command where

import qualified Data.Text as StrictText

import Gerber.ApertureDefinition ( ApertureDefinition )
import Gerber.DCodeNumber ( DCodeNumber )
import Gerber.Format ( Format )
import Gerber.Movement ( Movement )
import Gerber.Polarity ( Polarity )
import Gerber.Unit ( Unit )


data Command
  = FS !Format !Format
  | AD !DCodeNumber !ApertureDefinition
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
  | G75
  | G71
  | IP
  | LP !Polarity
  | MO !Unit
  | OF !( Maybe Float ) !( Maybe Float )
  | AM
  | SR !Movement
  deriving ( Eq, Show )
