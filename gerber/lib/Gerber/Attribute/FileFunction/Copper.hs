{-# language OverloadedStrings #-}

module Gerber.Attribute.FileFunction.Copper
  ( Mark(..), parseMark
  , Type(..), parseType
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )

-- text
import Data.Text ( unpack )


data Mark = Top | Inner | Bottom
  deriving ( Eq, Show )


parseMark :: MonadFail m => Field -> m Mark
parseMark field = case field of
  "Top" -> pure Top
  "Inr" -> pure Inner
  "Bot" -> pure Bottom
  _ -> fail $ "Bad Copper.Mark: " <> unpack field


data Type = Plane | Signal | Mixed | Hatched
  deriving ( Eq, Show )


parseType :: MonadFail m => Field -> m Type
parseType field = case field of
  "Plane" -> pure Plane
  "Signal" -> pure Signal
  "Mixed" -> pure Mixed
  "Hatched" -> pure Hatched
  _ -> fail $ "Bad Coppper.Type: " <> unpack field
