{-# language DeriveGeneric #-}

module Gerber.Evaluate.GraphicsState where

import Data.Monoid ( Dual, First, Last )
import Data.Monoid.Deletable ( Deletable )
import Data.Sequence (Seq)
import GHC.Generics ( Generic )
import Generics.Deriving.Monoid ( memptydefault, mappenddefault )

import qualified Data.IntMap.Strict as IntMap

import qualified Gerber.ApertureDefinition as Gerber
import qualified Gerber.DCodeNumber as Gerber
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.Evaluate.GraphicsState.InterpolationMode as Gerber
import qualified Gerber.Format as Gerber
import qualified Gerber.Padding as Gerber
import qualified Gerber.Polarity as Gerber
import qualified Gerber.StepRepeat as Gerber
import qualified Gerber.Unit as Gerber


data GraphicsState m = GraphicsState
  { coordinateSystem :: !( First ( Gerber.Padding, Gerber.Format, Gerber.Format ) )
  , unit :: !( First Gerber.Unit )
  , apertureDictionary :: !( IntMap.IntMap Gerber.ApertureDefinition )
  , currentAperture :: !( Last Gerber.DCodeNumber )
  , interpolationMode :: !( Last Gerber.InterpolationMode )
  , currentPoint :: !( Last ( Float, Float ) )
  , inRegion :: !( Last Bool )
  , polarity :: !( Last Gerber.Polarity )
  , currentContour :: !( Deletable ( First ( Float, Float ), Seq Edge.Edge ) )
  , stepRepeat :: !( Deletable ( First Gerber.StepRepeat, Dual m ) )
  }
  deriving ( Generic )

instance Monoid m => Semigroup ( GraphicsState m ) where
  (<>) =
    mappenddefault

instance Monoid m => Monoid ( GraphicsState m ) where
  mempty =
    memptydefault


initialGraphicsState :: Monoid m => GraphicsState m
initialGraphicsState =
  mempty
    { polarity = pure Gerber.Dark
    , currentPoint = pure ( 0, 0 )
    }
