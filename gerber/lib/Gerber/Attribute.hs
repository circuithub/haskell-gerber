{-# language OverloadedStrings #-}

module Gerber.Attribute
  ( FileAttribute(..)
  , parseFileAttribute
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Attribute( Attribute ) )
import Gerber.Attribute.CreationDate ( CreationDate, parseCreationDate )
import Gerber.Attribute.FileFunction ( FileFunction, parseFileFunction )
import Gerber.Attribute.FilePolarity ( FilePolarity, parseFilePolarity )
import Gerber.Attribute.GenerationSoftware ( GenerationSoftware, parseGenerationSoftware )
import Gerber.Attribute.MD5 ( MD5, parseMD5 )
import Gerber.Attribute.Part ( Part, parsePart )
import Gerber.Attribute.ProjectId ( ProjectId, parseProjectId )


data FileAttribute
  = Part !Part
  | FileFunction !FileFunction
  | FilePolarity !FilePolarity
  | GenerationSoftware !GenerationSoftware
  | CreationDate !CreationDate
  | ProjectId !ProjectId
  | MD5 !MD5
  | UserAttribute !Attribute
  deriving ( Eq, Show )


parseFileAttribute :: MonadFail m => Attribute -> m FileAttribute
parseFileAttribute attribute@(Attribute name fields) = case name of
  ".Part" -> Part <$> parsePart fields
  ".FileFunction" -> FileFunction <$> parseFileFunction fields
  ".FilePolarity" -> FilePolarity <$> parseFilePolarity fields
  ".GenerationSoftware" -> GenerationSoftware <$> parseGenerationSoftware fields
  ".CreationDate" -> CreationDate <$> parseCreationDate fields
  ".ProjectId" -> ProjectId <$> parseProjectId fields
  ".MD5" -> MD5 <$> parseMD5 fields
  _ -> pure (UserAttribute attribute)
