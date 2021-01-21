{-# language OverloadedStrings #-}

module Gerber.Attribute.FileFunction
  ( FileFunction(..), parseFileFunction
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )
import Gerber.Attribute.FileFunction.Types
  ( Copper, parseCopper
  , Drill, parseDrill
  , Mask, parseMask
  , Profile, parseProfile
  , Side, parseSide
  )

-- text
import Data.Text ( unpack )


data FileFunction
  = Copper !Copper
  | Soldermask !Mask
  | Legend !Mask
  | Goldmask !Mask
  | Silvermask !Mask
  | Tinmask !Mask
  | Carbonmask !Mask
  | Peelablesoldermask !Mask
  | Glue !Mask
  | Viatenting !Side
  | Viafill
  | Heatsink !Side
  | Paste !Side
  | KeepOut !Side
  | Pads !Side
  | Scoring !Side
  | Plated !Drill
  | NonPlated !Drill
  | Profile !Profile
  | Drillmap
  | FabricationDrawing
  | ArrayDrawing
  | AssemblyDrawing !Side
  | Drawing !Field
  | Other !Field
  deriving ( Eq, Show )


parseFileFunction :: MonadFail m => [Field] -> m FileFunction
parseFileFunction [] = fail "Bad .FileFunction: at least 1 field required"
parseFileFunction (name : values) = case name of
  "Copper" -> Copper <$> arity2or3 parseCopper
  "Soldermask" -> Soldermask <$> arity1or2 parseMask
  "Legend" -> Legend <$> arity1or2 parseMask
  "Goldmask" -> Goldmask <$> arity1or2 parseMask
  "Silvermask" -> Silvermask <$> arity1or2 parseMask
  "Tinmask" -> Tinmask <$> arity1or2 parseMask
  "Carbonmask" -> Legend <$> arity1or2 parseMask
  "Peelablasoldermask" -> Peelablesoldermask <$> arity1or2 parseMask
  "Glue" -> Glue <$> arity1or2 parseMask
  "Viatenting" -> Viatenting <$> arity1 parseSide
  "Viafill" -> arity0 $ pure Viafill
  "Heatsink" -> Heatsink <$> arity1 parseSide
  "Paste" -> Paste <$> arity1 parseSide
  "Keep-out" -> KeepOut <$> arity1 parseSide
  "Scoring" -> Scoring <$> arity1 parseSide
  "Plated" -> Plated <$> arity3or4 parseDrill
  "NonPlated" -> NonPlated <$> arity3or4 parseDrill
  "Profile" -> Profile <$> arity1 parseProfile
  "Drillmap" -> arity0 $ pure Drillmap
  "FabricationDrawing" -> arity0 $ pure FabricationDrawing
  "ArrayDrawing" -> arity0 $ pure ArrayDrawing
  "AssemblyDrawing" -> AssemblyDrawing <$> arity1 parseSide
  "Drawing" -> Drawing <$> arity1 pure
  "Other" -> Other <$> arity1 pure
  _ -> fail $ "Bad .FileFunction: unknown value " <> unpack name
  where
    arity0 f = case values of
      [] -> f
      _ -> fail $ message "0"

    arity1 f = case values of
      [a] -> f a
      _ -> fail $ message "1"

    arity1or2 f = case values of
      [a, b] -> f a (Just b)
      [a] -> f a Nothing
      _ -> fail $ message "1 or 2"

    arity2or3 f = case values of
      [a, b, c] -> f a b (Just c)
      [a, b] -> f a b Nothing
      _ -> fail $ message "2 or 3"

    arity3or4 f = case values of
      [a, b, c, d] -> f a b c (Just d)
      [a, b, c] -> f a b c Nothing
      _ -> fail $ message "3 or 4"

    message n =
      "Bad .FileFunction: " <>
      unpack name <>
      " field requires " <>
      n <>
      " values"
