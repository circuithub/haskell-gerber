{-# LANGUAGE OverloadedStrings #-}

module Gerber.Attribute.FilePolarity (
  FilePolarity (..),
) where


data FilePolarity
  = Positive
  | Negative
  deriving (Eq, Show)
