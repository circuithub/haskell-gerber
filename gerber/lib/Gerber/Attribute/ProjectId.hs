{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module Gerber.Attribute.ProjectId
  ( ProjectId(..), parseProjectId
  ) where

-- base16-bytestring
import Data.ByteString.Base16 ( decode )

-- bytestring
import Data.ByteString.Lazy ( fromStrict )

-- gerber
import Gerber.Attribute.Attribute ( Field )

-- text
import Data.Text.Encoding ( encodeUtf8 )

-- uuid-types
import Data.UUID.Types ( UUID, fromByteString )


data ProjectId = ProjectId
  { name :: !Field
  , guid :: !UUID
  , revision :: !Field
  }
  deriving ( Eq, Show )


parseProjectId :: MonadFail m => [Field] -> m ProjectId
parseProjectId fields = case fields of
  [name, uuid, revision] ->
    ProjectId name <$> parseUUID uuid <*> pure revision
  _ -> fail "Bad .ProjectId: must have exactly 3 fields"


parseUUID :: MonadFail m => Field -> m UUID
parseUUID hextets = case decode (encodeUtf8 hextets) of
  (fromByteString . fromStrict -> Just uuid, "") -> pure uuid
  _ -> fail "Bad .ProjectId: could not parse UUID from project guid"
