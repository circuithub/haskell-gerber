{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Gerber.Command (
  module Gerber.Command,
) where

-- gerber
import Gerber.ApertureDefinition (ApertureDefinition)
import Gerber.Attribute (FileAttribute)
import Gerber.DCodeNumber (DCodeNumber)
import Gerber.Format (Format)
import Gerber.MacroDefinition (Definition, Modifier)
import Gerber.Mirroring (Mirroring)
import Gerber.Movement (Movement)
import Gerber.Padding (Padding)
import Gerber.Polarity (Polarity)
import Gerber.StepRepeat (StepRepeat)
import Gerber.Unit (Unit)

-- text
import qualified Data.Text as StrictText


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
  | OF !(Maybe Float) !(Maybe Float)
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
  | TF !FileAttribute
  | TO !StrictText.Text ![StrictText.Text]
  | TA !StrictText.Text ![StrictText.Text]
  | TD !(Maybe StrictText.Text)
  | LN !StrictText.Text
  deriving (Eq, Show)
