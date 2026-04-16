{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Gerber.Attribute.ProjectId (
  ProjectId (..),
  projectIdParser,
) where

-- base16-bytestring
import Data.ByteString.Base16 (decode)

-- bytestring
import qualified Data.ByteString.Lazy

-- containers
import qualified Data.Set as Set

-- gerber
import Gerber.Attribute.Attribute (Field)

-- megaparsec
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Text.Megaparsec.Error

-- text
import qualified Data.Text as StrictText
import qualified Data.Text.Lazy as LazyText

-- uuid-types
import Data.UUID.Types (UUID, fromText)


data ProjectId = ProjectId
  { name :: !Field
  , guid :: !UUID
  , revision :: !Field
  }
  deriving (Eq, Show)


projectIdParser :: Megaparsec.MonadParsec e StrictText.Text m => m ProjectId
projectIdParser = do
  _ <- Megaparsec.string "ProjectId"
  name <- field
  guid <- field
  revision <- field

  case fromText guid of
    Nothing -> Megaparsec.fancyFailure (Set.singleton (ErrorFail "Invalid UUID provided in ProjectId information"))
    Just uuid -> pure $ ProjectId name uuid revision
  where
    attributeValue :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
    attributeValue =
      StrictText.pack <$> Megaparsec.some (Megaparsec.noneOf [',', '*', '%'])

    field :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
    field = Megaparsec.char ',' *> attributeValue
