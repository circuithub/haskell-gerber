{-# language OverloadedStrings #-}

module Gerber.Attribute.MD5
  ( MD5(..), parseMD5
  ) where

-- base16-bytestring
import Data.ByteString.Base16 ( decode )

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString as ByteString

-- gerber
import Gerber.Attribute.Attribute ( Field )

-- text
import Data.Text.Encoding ( encodeUtf8 )


newtype MD5 = MD5 ByteString
  deriving ( Eq, Show )


parseMD5 :: MonadFail m => [Field] -> m MD5
parseMD5 fields = case fields of
  [field] -> case decode (encodeUtf8 field) of
    (bytes, "") | ByteString.length bytes == 16 -> pure (MD5 bytes)
    _ -> fail "Bad .MD5: must consist of exactly 32 hexadecimal digits"
  _ -> fail "Bad .MD5: must have exactly 1 field"
