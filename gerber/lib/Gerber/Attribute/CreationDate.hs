{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Gerber.Attribute.CreationDate (
  CreationDate (..),
  creationDateParser,
) where

-- base
import Control.Applicative (many, (<|>))

-- containers
import qualified Data.Set as Set

-- gerber
import Gerber.Attribute.Attribute (Field)

-- megaparsec
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Text.Megaparsec.Error

-- text
import Data.Text (unpack)
import qualified Data.Text as StrictText

-- time
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)


newtype CreationDate = CreationDate UTCTime
  deriving (Eq, Show)


creationDateParser :: Megaparsec.MonadParsec e StrictText.Text m => m CreationDate
creationDateParser = do
  _ <- Megaparsec.string "CreationDate,"
  timeText <- many (Megaparsec.noneOf ['*', '%'])

  let parseISO8601 t =
        parseTimeM True defaultTimeLocale "%FT%T%QZ" t
          <|> parseTimeM True defaultTimeLocale "%FT%T%Q%z" t

  case parseISO8601 timeText of
    Nothing -> Megaparsec.fancyFailure (Set.singleton (ErrorFail $ "Cannot parse creation date: " ++ timeText))
    Just x -> pure (CreationDate x)
