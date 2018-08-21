{-# language DeriveGeneric #-}

module Gerber.Evaluate.GraphicsState where

import Data.Monoid ( First, Last )
import GHC.Generics ( Generic )
import Generics.Deriving.Monoid ( memptydefault, mappenddefault )
import Data.Monoid.Deletable ( Deletable )

import qualified Data.IntMap.Strict as IntMap

import qualified Gerber.ApertureDefinition as Gerber
import qualified Gerber.DCodeNumber as Gerber
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.Evaluate.GraphicsState.InterpolationMode as Gerber
import qualified Gerber.Format as Gerber
import qualified Gerber.Polarity as Gerber
import qualified Gerber.Unit as Gerber


data GraphicsState = GraphicsState
  { coordinateSystem :: !( First ( Gerber.Format, Gerber.Format ) )
  , unit :: !( First Gerber.Unit )
  , apertureDictionary :: !( IntMap.IntMap Gerber.ApertureDefinition )
  , currentAperture :: !( Last Gerber.DCodeNumber )
  , interpolationMode :: !( Last Gerber.InterpolationMode )
  , currentPoint :: !( Last ( Float, Float ) )
  , inRegion :: !( Last Bool )
  , polarity :: !( Last Gerber.Polarity )
  , currentContour :: Deletable ( First ( Float, Float ), [ Edge.Edge ] )
  }
  deriving ( Generic )

instance Semigroup GraphicsState where
  (<>) =
    mappenddefault

instance Monoid GraphicsState where
  mempty =
    memptydefault


initialGraphicsState :: GraphicsState
initialGraphicsState =
  mempty
    { polarity = pure Gerber.Dark
    , currentPoint = pure ( 0, 0 )
    }
