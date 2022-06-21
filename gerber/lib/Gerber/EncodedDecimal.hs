{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}

module Gerber.EncodedDecimal ( EncodedDecimal(EncodedDecimal, StoredEncodedDecimal), negative, digits ) where

import Data.Int
import Data.Word
import Data.List (foldl')

pattern EncodedDecimal :: Bool -> [ Int ] -> EncodedDecimal
pattern EncodedDecimal {negative, digits} <- (toEncoded -> (negative, digits)) where
  EncodedDecimal n d = fromEncoded n d

{-# COMPLETE EncodedDecimal #-}

data EncodedDecimal =
  StoredEncodedDecimal
    { width :: {-# UNPACK #-} !Word8
    , digitsAsInt :: {-# UNPACK #-} !Int32
    }
  deriving
    ( Eq )

instance Show EncodedDecimal where
   show EncodedDecimal{negative = n, digits = d} =
    "EncodedDecimal {negative = " ++ show n ++ ", digits = " ++ show d ++ "}"


toEncoded :: EncodedDecimal -> (Bool, [ Int ])
toEncoded StoredEncodedDecimal {width, digitsAsInt} = (digitsAsInt < 0, reverse (go 0 (fromIntegral (abs digitsAsInt))))
  where
    go :: Word8 -> Int -> [Int]
    go !currentWidth !num
      | currentWidth == width = []
      | otherwise = num `mod` 10 : go (currentWidth + 1) (num `div` 10)

fromEncoded :: Bool -> [Int] -> EncodedDecimal
fromEncoded isNegative = doNegation . foldl' step zero . reverse
  where
    zero = StoredEncodedDecimal 0 0
    step StoredEncodedDecimal{ width, digitsAsInt } digit = StoredEncodedDecimal
      { width = width + 1
      , digitsAsInt = digitsAsInt + fromIntegral digit * 10 ^ width
      }
    doNegation
      | isNegative = \x@StoredEncodedDecimal{digitsAsInt} -> x {digitsAsInt = negate digitsAsInt}
      | otherwise = id
