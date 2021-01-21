{-# language OverloadedStrings #-}

module Gerber.Attribute.FilePolarity
  ( FilePolarity(..), parseFilePolarity
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )

-- text
import Data.Text ( unpack )


data FilePolarity
  = Positive
  | Negative
  deriving ( Eq, Show )


parseFilePolarity :: MonadFail m => [Field] -> m FilePolarity
parseFilePolarity [field] = case field of
  "Positive" -> pure Positive
  "Negative" -> pure Negative
  _ -> fail $ "Bad .FilePolarity: unknown value " <> unpack field
parseFilePolarity _ = fail "Bad .FilePolarity: must have exactly 1 field"
