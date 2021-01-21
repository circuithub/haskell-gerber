{-# language NamedFieldPuns #-}

module Gerber.Attribute.GenerationSoftware
  ( GenerationSoftware(..), parseGenerationSoftware
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )


data GenerationSoftware = GenerationSoftware
  { vendor :: !Field
  , application :: !Field
  , version :: !(Maybe Field)
  }
  deriving ( Eq, Show )


parseGenerationSoftware :: MonadFail m => [Field] -> m GenerationSoftware
parseGenerationSoftware fields = case fields of
  [vendor, application] ->
    pure $ GenerationSoftware vendor application Nothing
  [vendor, application, version] ->
    pure $ GenerationSoftware vendor application (Just version)
  _ -> fail $ "Bad .GenerationSoftware: exactly 2 or 3 fields required"
