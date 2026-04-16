{-# LANGUAGE OverloadedStrings #-}

module Gerber.Attribute.FilePolarity (
  FilePolarity (..),
) where

-- gerber
import Gerber.Attribute.Attribute (Field)

-- text
import Data.Text (unpack)


data FilePolarity
  = Positive
  | Negative
  deriving (Eq, Show)
