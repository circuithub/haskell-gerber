{-# language DeriveGeneric #-}

module Gerber.Evaluate.GraphicsState where

import Data.Monoid ( Dual, First, Last )
import Data.Monoid.Deletable ( Deletable )
import Data.Sequence (Seq)
import GHC.Generics ( Generic )
import Generics.Deriving.Monoid ( memptydefault, mappenddefault )
import Data.Text  (Text)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import qualified Gerber.Command as Gerber
import qualified Gerber.DCodeNumber as Gerber
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.Evaluate.GraphicsState.ApertureEntry as Gerber
import qualified Gerber.Evaluate.GraphicsState.InterpolationMode as Gerber
import qualified Gerber.Evaluate.GraphicsState.StackStream as Gerber
import qualified Gerber.Format as Gerber
import qualified Gerber.MacroDefinition as MacroDefinition
import qualified Gerber.Mirroring as Gerber
import qualified Gerber.Padding as Gerber
import qualified Gerber.Polarity as Gerber
import qualified Gerber.StepRepeat as Gerber
import qualified Gerber.Unit as Gerber


data GraphicsState m = GraphicsState
  { coordinateSystem :: !( First ( Gerber.Padding, Gerber.Format, Gerber.Format ) )
  , unit :: !( First Gerber.Unit )
  , apertureDictionary :: !( IntMap.IntMap Gerber.ApertureEntry )
  , macroDictionary :: !( Map.Map Text [MacroDefinition.Definition MacroDefinition.Modifier MacroDefinition.Modifier] )
  , currentAperture :: !( Last Gerber.DCodeNumber )
  , interpolationMode :: !( Last Gerber.InterpolationMode )
  , currentPoint :: !( Last ( Float, Float ) )
  , inRegion :: !( Last Bool )
  , polarity :: !( Last Gerber.Polarity )
  , currentContour :: !( Deletable ( First ( Float, Float ), Seq Edge.Edge ) )
  , stepRepeat :: !( Deletable ( First Gerber.StepRepeat, Dual m ) )
  , mirror :: !( Last Gerber.Mirroring )
  , scale :: !( Last Float )
  , rotate :: !( Last Float )
  , blockApertureStack :: !( Gerber.StackStream Gerber.DCodeNumber Gerber.Command )
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

