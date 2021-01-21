{-# language OverloadedStrings #-}

module Gerber.Attribute.FileFunction.Drill
  ( Type(..), parseType
  , Via(..), parseVia
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )

-- text
import Data.Text ( unpack )


data Type = Drill | Route | Mixed
  deriving ( Eq, Show )


parseType :: MonadFail m => Field -> m Type
parseType field = case field of
  "Drill" -> pure Drill
  "Route" -> pure Route
  "Mixed" -> pure Mixed
  _ -> fail $ "Bad Drill.Type: " <> unpack field


data Via = TH | Blind | Buried
  deriving ( Eq, Show )


parseVia :: MonadFail m => Field -> m Via
parseVia field = case field of
  "PTH" -> pure TH
  "NPTH" -> pure TH
  "Blind" -> pure Blind
  "Buried" -> pure Buried
  _ -> fail $ "Bad Drill.Via: " <> unpack field
