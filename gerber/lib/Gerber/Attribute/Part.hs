{-# language OverloadedStrings #-}

module Gerber.Attribute.Part
  ( Part(..)
  , parsePart
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )


data Part
  = Single
  | Array
  | FabricationPanel
  | Coupon
  | Other !Field
  deriving ( Eq, Show )


parsePart :: MonadFail m => [Field] -> m Part
parsePart fields = case fields of
  [field] -> pure $ case field of
    "Single" -> Single
    "Array" -> Array
    "FabricationPanel" -> FabricationPanel
    "Coupon" -> Coupon
    _ -> Other field
  _ -> fail "Bad .Part: must have exactly 1 field"
