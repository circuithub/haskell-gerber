{-# LANGUAGE DeriveGeneric #-}

module Gerber.Evaluate.GraphicsState where

-- base
import Data.Monoid (Dual (Dual), First, Last)
import GHC.Generics (Generic)

-- containers
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)

-- generic-deriving
import Generics.Deriving.Monoid (mappenddefault, memptydefault)

-- gerber
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

-- monoid-extras
import Data.Monoid.Deletable (Deletable (Deletable))

-- text
import Data.Text (Text)


data GraphicsState m = GraphicsState
  { coordinateSystem :: !(First (Gerber.Padding, Gerber.Format, Gerber.Format))
  , unit :: !(First Gerber.Unit)
  , apertureDictionary :: !(IntMap.IntMap Gerber.ApertureEntry)
  , macroDictionary :: !(Map.Map Text [MacroDefinition.Definition MacroDefinition.Modifier MacroDefinition.Modifier])
  , currentAperture :: !(Last Gerber.DCodeNumber)
  , interpolationMode :: !(Last Gerber.InterpolationMode)
  , currentPoint :: !(Last (Double, Double))
  , inRegion :: !(Last Bool)
  , polarity :: !(Last Gerber.Polarity)
  , currentContour :: !(Deletable (First (Double, Double), Seq Edge.Edge))
  , stepRepeat :: !(Deletable (First Gerber.StepRepeat, Dual m))
  , mirror :: !(Last Gerber.Mirroring)
  , scale :: !(Last Double)
  , rotate :: !(Last Double)
  , blockApertureStack :: !(Gerber.StackStream Gerber.DCodeNumber Gerber.Command)
  }
  deriving (Generic)


-- The evaluator merges a state into the last after every command. The open
-- region and step and repeat block wrap lazy pairs, which those merges would
-- otherwise chain into thunks the length of the gerber.
instance Monoid m => Semigroup (GraphicsState m) where
  a <> b =
    case mappenddefault a b of
      merged@GraphicsState{currentContour = Deletable _ (start, edges) _, stepRepeat = Deletable _ (block, Dual drawing) _} ->
        start `seq` edges `seq` block `seq` drawing `seq` merged


instance Monoid m => Monoid (GraphicsState m) where
  mempty =
    memptydefault


initialGraphicsState :: Monoid m => GraphicsState m
initialGraphicsState =
  mempty
    { polarity = pure Gerber.Dark
    , currentPoint = pure (0, 0)
    }
