{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Gerber.EncodedDecimal (
  EncodedDecimal (EncodedDecimal, StoredEncodedDecimal),
  negative,
  digits,
  fromDigitText,
  decodeEncodedDecimal,
) where

-- base
import Data.Char (ord)
import Data.Int
import Data.List (foldl')
import Data.Word

-- gerber
import Gerber.Format (Format (Format, decimalPositions, integerPositions))
import Gerber.Padding (Padding (PadLeading, PadTrailing))

-- text
import qualified Data.Text as Text


pattern EncodedDecimal :: Bool -> [Int] -> EncodedDecimal
pattern EncodedDecimal{negative, digits} <- (toEncoded -> (negative, digits))
  where
    EncodedDecimal n d = fromEncoded n d


{-# COMPLETE EncodedDecimal #-}


-- | The digits of a coordinate as written: how many there are, and their value, negated for a negative coordinate.
data EncodedDecimal = StoredEncodedDecimal
  { width :: {-# UNPACK #-} !Word8
  , digitsAsInt :: {-# UNPACK #-} !Int64
  }
  deriving
    (Eq)


instance Show EncodedDecimal where
  show EncodedDecimal{negative = n, digits = d} =
    "EncodedDecimal {negative = " ++ show n ++ ", digits = " ++ show d ++ "}"


toEncoded :: EncodedDecimal -> (Bool, [Int])
toEncoded StoredEncodedDecimal{width, digitsAsInt} = (digitsAsInt < 0, reverse (go 0 (fromIntegral (abs digitsAsInt))))
  where
    go :: Word8 -> Int -> [Int]
    go !currentWidth !num
      | currentWidth == width = []
      | otherwise = num `mod` 10 : go (currentWidth + 1) (num `div` 10)


fromEncoded :: Bool -> [Int] -> EncodedDecimal
fromEncoded isNegative = doNegation . foldl' step zero . reverse
  where
    zero = StoredEncodedDecimal 0 0
    step StoredEncodedDecimal{width, digitsAsInt} digit =
      StoredEncodedDecimal
        { width = width + 1
        , digitsAsInt = digitsAsInt + fromIntegral digit * 10 ^ width
        }
    doNegation
      | isNegative = \x@StoredEncodedDecimal{digitsAsInt} -> x{digitsAsInt = negate digitsAsInt}
      | otherwise = id


-- | Like 'EncodedDecimal' from the text of the digits, which must all be ASCII digits.
fromDigitText :: Bool -> Text.Text -> EncodedDecimal
fromDigitText isNegative text =
  StoredEncodedDecimal
    { width = fromIntegral (Text.length text)
    , digitsAsInt = (if isNegative then negate else id) (Text.foldl' (\value digit -> value * 10 + fromIntegral (ord digit - ord '0')) 0 text)
    }


{- | The coordinate the digits stand for in the format. Omitted leading zeros
are restored on the left and omitted trailing zeros on the right, where
digits beyond the format are dropped.

The integer and the power of ten are both exact, so the one division rounds
correctly: the result is the 'Double' nearest the decimal coordinate.
-}
decodeEncodedDecimal :: Padding -> Format -> EncodedDecimal -> Double
decodeEncodedDecimal padding Format{integerPositions, decimalPositions} StoredEncodedDecimal{width, digitsAsInt} =
  signum (fromIntegral digitsAsInt) * (fromIntegral scaled / 10 ^ decimalPositions)
  where
    magnitude = abs digitsAsInt
    positions = integerPositions + decimalPositions
    scaled = case padding of
      PadLeading -> magnitude
      PadTrailing
        | fromIntegral width <= positions -> magnitude * 10 ^ (positions - fromIntegral width)
        | otherwise -> magnitude `div` 10 ^ (fromIntegral width - positions)
