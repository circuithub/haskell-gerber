{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Gerber.Attribute.FileFunction.Types
  ( Copper(..), parseCopper
  , Drill(..), parseDrill
  , Index(..), parseIndex
  , Mask(..), parseMask
  , Profile(..), parseProfile
  , Side(..), parseSide
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )
import qualified Gerber.Attribute.FileFunction.Copper as Copper
import qualified Gerber.Attribute.FileFunction.Drill as Drill

-- text
import Data.Text ( uncons, unpack )
import Data.Text.Read ( decimal )


data Copper = Copper
  { index :: !Index
  , mark :: !Copper.Mark
  , type_ :: !(Maybe Copper.Type)
  }
  deriving ( Eq, Show )


parseCopper :: MonadFail m => Field -> Field -> Maybe Field -> m Copper
parseCopper position mark type_ =
  Copper
    <$> parsePosition position
    <*> Copper.parseMark mark
    <*> traverse Copper.parseType type_
  where
    parsePosition field = case uncons field of
      Just ('L', index) -> parseIndex index
      _ -> fail "Bad Copper: position must begin with L"


data Drill = Drill
  { from :: !Index
  , to :: !Index
  , via :: !Drill.Via
  , type_ :: !(Maybe Drill.Type)
  }
  deriving ( Eq, Show )


parseDrill :: MonadFail m => Field -> Field -> Field -> Maybe Field -> m Drill
parseDrill from to via type_ =
  Drill
    <$> parseIndex from
    <*> parseIndex to
    <*> Drill.parseVia via
    <*> traverse Drill.parseType type_


newtype Index = Index Word
  deriving ( Eq, Show )


parseIndex :: MonadFail m => Field -> m Index
parseIndex field = case decimal field of
  Right (n, "")
    | n >= 1 -> pure (Index n)
    | otherwise -> fail "Bad Index: must be at least 1"
  Right (_, rest) -> fail $ "Bad Index: non-numeric suffix " <> unpack rest
  Left err -> fail $ "Bad Index: " <> err


data Mask = Mask
  { side :: !Side
  , index :: !(Maybe Index)
  }
  deriving ( Eq, Show )


parseMask :: MonadFail m => Field -> Maybe Field -> m Mask
parseMask side index =
  Mask
    <$> parseSide side
    <*> traverse parseIndex index


data Profile = Plated | NonPlated
  deriving ( Eq, Show )


parseProfile :: MonadFail m => Field -> m Profile
parseProfile field = case field of
  "P" -> pure Plated
  "NP" -> pure NonPlated
  _ -> fail $ "Bad Profile: " ++ unpack field


data Side = Top | Bottom
  deriving ( Eq, Show )


parseSide :: MonadFail m => Field -> m Side
parseSide field = case field of
  "Top" -> pure Top
  "Bot" -> pure Bottom
  _ -> fail $ "Bad Side: " ++ unpack field
