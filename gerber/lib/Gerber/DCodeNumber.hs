{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gerber.DCodeNumber where


newtype DCodeNumber = DCodeNumber Int
  deriving newtype (Eq, Ord, Read, Show)
