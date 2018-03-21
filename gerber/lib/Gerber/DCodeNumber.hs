{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}
module Gerber.DCodeNumber where

newtype DCodeNumber = DCodeNumber Int
  deriving newtype (Eq, Ord, Read, Show)
