{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Gerber.Attribute.MD5 (
  MD5 (..),
  parser,
) where

-- base16-bytestring
import Data.ByteString.Base16 (decode)

-- bytestring
import qualified Data.ByteString as ByteString

-- containers
import qualified Data.Set as Set

-- megaparsec
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

-- text
import qualified Data.Text as StrictText
import Data.Text.Encoding (encodeUtf8)


newtype MD5 = MD5 StrictText.Text
  deriving (Eq, Show)


parser :: Megaparsec.MonadParsec e StrictText.Text m => m MD5
parser = do
  _ <- Megaparsec.string "MD5,"
  hash <- StrictText.pack <$> Megaparsec.many (Megaparsec.noneOf ['*', '%'])

  case decode (encodeUtf8 hash) of
    Right bytes | ByteString.length bytes == 16 -> pure (MD5 hash)
    _ -> Megaparsec.fancyFailure (Set.singleton (Megaparsec.ErrorFail "Bad .MD5: must consist of exactly 32 hexadecimal digits"))
