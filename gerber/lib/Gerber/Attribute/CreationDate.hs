module Gerber.Attribute.CreationDate
  ( CreationDate(..), parseCreationDate
  ) where

-- gerber
import Gerber.Attribute.Attribute ( Field )

-- text
import Data.Text ( unpack )

-- time
import Data.Time.Clock ( UTCTime )
import Data.Time.Format.ISO8601 ( formatParseM, iso8601Format )


newtype CreationDate = CreationDate UTCTime
  deriving ( Eq, Show )


parseCreationDate :: MonadFail m => [Field] -> m CreationDate
parseCreationDate fields = case fields of
  [field] -> CreationDate <$> formatParseM iso8601Format (unpack field)
  _ -> fail "Bad .CreationDate: must have exactly 1 field"
