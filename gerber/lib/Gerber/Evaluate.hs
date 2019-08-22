{-# language DisambiguateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}

module Gerber.Evaluate where

import Data.Char ( intToDigit )
import GHC.Stack ( HasCallStack )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>), First(..), getLast )
import Data.Monoid.Deletable ( deleteR, toDeletable, unDelete )

import qualified Control.Foldl as Fold
import qualified Data.IntMap.Strict as IntMap

import qualified Gerber.ApertureDefinition as ApertureDefinition
import qualified Gerber.Command as Command
import qualified Gerber.DCodeNumber as DCodeNumber
import qualified Gerber.EncodedDecimal as EncodedDecimal
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.Evaluate.GraphicsState as GraphicsState
import qualified Gerber.Evaluate.GraphicsState.InterpolationMode as InterpolationMode
import qualified Gerber.Format as Format
import qualified Gerber.Movement as Movement
import qualified Gerber.Padding as Padding
import qualified Gerber.Polarity as Polarity
import qualified Gerber.StepRepeat as StepRepeat
import qualified Gerber.Unit as Unit


data Evaluator m = Evaluator
  { line ::
      Polarity.Polarity
        -> ApertureDefinition.ApertureDefinition
        -> (Float, Float)
        -> (Float, Float)
        -> m
  , flash ::
      Polarity.Polarity
        -> ApertureDefinition.ApertureDefinition
        -> (Float, Float)
        -> m
  , fillRegion ::
      Polarity.Polarity
        -> ( Float, Float )
        -> [ Edge.Edge ]
        -> m
  , arc ::
      Polarity.Polarity
        -> ApertureDefinition.ApertureDefinition
        -> (Float, Float)
        -> (Float, Float)
        -> (Float, Float)
        -> m
  , translate ::
      Float
        -> Float
        -> m
        -> m
  }


evaluate :: Monoid m => Evaluator m -> Fold.Fold Command.Command m
evaluate evaluator =
  Fold.Fold
    ( \( drawing, state) command ->
        let
          ( d, s ) =
            step evaluator state command

        in
        ( d <> drawing, state <> s )
    )
    ( mempty, GraphicsState.initialGraphicsState )
    fst


step
  :: forall m.
     Monoid m
  => Evaluator m
  -> GraphicsState.GraphicsState m
  -> Command.Command
  -> ( m, GraphicsState.GraphicsState m )
step evaluator state = \case
  Command.FS padding xfmt yfmt ->
    ( mempty
    , mempty { GraphicsState.coordinateSystem = pure ( padding, xfmt, yfmt ) }
    )

  Command.AD ( DCodeNumber.DCodeNumber n ) def ->
    let
      scaleRectParams params =
        params
          { ApertureDefinition.width =
              toMM state ( ApertureDefinition.width params )
          , ApertureDefinition.height =
              toMM state ( ApertureDefinition.height params )
          }

      defInMM =
        case def of
          ApertureDefinition.Circle params ->
            ApertureDefinition.Circle
              params
                { ApertureDefinition.diameter =
                    toMM state ( ApertureDefinition.diameter params )
                }

          ApertureDefinition.Rectangle params ->
            ApertureDefinition.Rectangle ( scaleRectParams params )

          ApertureDefinition.Obround params ->
            ApertureDefinition.Obround ( scaleRectParams params )

          -- TODO
          other ->
            other

    in
    ( mempty
    , mempty
        { GraphicsState.apertureDictionary = IntMap.singleton n defInMM }
    )

  Command.D n ->
    ( mempty
    , mempty { GraphicsState.currentAperture = pure n }
    )

  Command.G04 _ ->
    mempty

  Command.MO units ->
    ( mempty
    , mempty { GraphicsState.unit = pure units }
    )

  Command.LP polarity ->
    ( mempty, mempty { GraphicsState.polarity = pure polarity } )

  Command.D01 to | inRegion state ->
    let
      ( newPoint, offset ) =
        moveTo state to

    in
    ( mempty
    , mempty
        { GraphicsState.currentContour =
            toDeletable
              ( mempty
              , [ let
                    center =
                      let
                        ( dx, dy ) =
                          offset

                      in
                      ( fst currentPoint + dx, snd currentPoint + dy )

                    currentPoint =
                      getCurrentPoint state

                  in
                  case currentInterpolationMode state of
                    InterpolationMode.Linear ->
                      Edge.Line newPoint

                    InterpolationMode.CircularCW ->
                      Edge.ArcCW
                        center
                        newPoint

                    InterpolationMode.CircularCCW ->
                      Edge.ArcCCW
                        center
                        newPoint
                ]
              )
        , GraphicsState.currentPoint =
            pure newPoint
        }
    )

  Command.D01 to | otherwise -> drawToImageOrStepRepeat $
    let
      interp =
        currentInterpolationMode state

      ( newPoint, offset ) =
        moveTo state to

      apertureDefinition =
        currentAperture state

      polarity =
        currentPolarity state

      currentPoint =
        getCurrentPoint state

      center =
        let
          ( dx, dy ) =
            offset

        in
        ( fst currentPoint + dx, snd currentPoint + dy )

    in
    ( case interp of
        InterpolationMode.Linear ->
          line
            evaluator
            polarity
            apertureDefinition
            currentPoint
            newPoint

        InterpolationMode.CircularCW ->
          arc evaluator polarity apertureDefinition currentPoint center newPoint

        InterpolationMode.CircularCCW ->
          arc evaluator polarity apertureDefinition currentPoint center newPoint
    , mempty { GraphicsState.currentPoint = pure newPoint }
    )

  Command.D02 to | inRegion state ->
    let
      ( newPoint, _ ) =
        moveTo state to

      polarity =
        currentPolarity state

    in
    ( case unDelete ( GraphicsState.currentContour state) of
        ( First ( Just start ), edges ) ->
          fillRegion evaluator polarity start edges

        _ ->
          error "Region finished without any edges"
    , mempty
        { GraphicsState.currentContour =
            deleteR <> toDeletable ( pure newPoint, mempty )
        , GraphicsState.currentPoint =
            pure newPoint
        }
    )

  Command.D02 to ->
    drawToImageOrStepRepeat $
    let
      ( newPoint, _ ) =
        moveTo state to

    in
    ( mempty
    , mempty { GraphicsState.currentPoint = pure newPoint }
    )

  Command.D03 to ->
    drawToImageOrStepRepeat $
    let
      ( newPoint, _ ) =
        moveTo state to

      apertureDefinition =
        currentAperture state

      polarity =
        currentPolarity state

    in
    ( flash evaluator polarity apertureDefinition newPoint
    , mempty { GraphicsState.currentPoint = pure newPoint }
    )

  Command.G36 ->
    ( mempty
    , mempty
        { GraphicsState.inRegion =
            pure True
        , GraphicsState.currentContour =
            deleteR <> toDeletable ( pure ( getCurrentPoint state ), mempty )
        }
    )

  Command.G37 {} ->
    drawToImageOrStepRepeat $
    let
      polarity =
        currentPolarity state

    in
    ( case unDelete ( GraphicsState.currentContour state ) of
        ( First ( Just start ), edges ) ->
          fillRegion evaluator polarity start edges

        _ ->
          mempty
    , mempty
        { GraphicsState.inRegion = pure False
        }
    )

  Command.IP{} ->
    mempty

  Command.G75{} ->
    mempty

  Command.G01 ->
    ( mempty
    , mempty
        { GraphicsState.interpolationMode = pure InterpolationMode.Linear }
    )

  Command.G02 ->
    ( mempty
    , mempty
        { GraphicsState.interpolationMode = pure InterpolationMode.CircularCW }
    )

  Command.G03 ->
    ( mempty
    , mempty
        { GraphicsState.interpolationMode = pure InterpolationMode.CircularCCW }
    )

  Command.G71 ->
    ( mempty, mempty { GraphicsState.unit = pure Unit.MM } )

  Command.OF ( Just 0 ) ( Just 0 ) ->
    mempty

  Command.AM ->
    mempty

  Command.SR movement ->
    flushSR
      ( mempty
      , ( mempty @( GraphicsState.GraphicsState m ) )
        { GraphicsState.stepRepeat = deleteR <> toDeletable ( pure movement, mempty ) }
      )

  Command.M02 ->
    flushSR mempty

  Command.SF ->
    mempty

  Command.MI ->
    mempty

  cmd ->
    error ( show cmd )

  where

    flushSR =
      let
        close =
          case unDelete ( GraphicsState.stepRepeat state ) of
            ( First ( Just movement ), drawing ) ->
              let
                StepRepeat.StepRepeat{ xRepeats, yRepeats, xStep, yStep } =
                  movement

              in
              ( mconcat
                  ( do
                      xi <-
                        [ 1 .. xRepeats ]

                      yi <-
                        [ 1 .. yRepeats ]

                      return
                        ( translate
                            evaluator
                            ( fromIntegral ( xi - 1 ) * toMM state xStep )
                            ( fromIntegral ( yi - 1 ) * toMM state yStep )
                            drawing
                        )
                  )
              , mempty
              )

            _ ->
              mempty


      in
      mappend close


    drawToImageOrStepRepeat ( drawing, stateChange ) =
      case unDelete ( GraphicsState.stepRepeat state ) of
        ( First Nothing, _ ) ->
          ( drawing, stateChange )

        ( _, _ ) ->
          ( mempty
          , stateChange <> ( mempty @( GraphicsState.GraphicsState m ) ) { GraphicsState.stepRepeat = toDeletable ( mempty, drawing ) }
          )


moveTo
  :: GraphicsState.GraphicsState m
  -> Movement.Movement
  -> ( ( Float, Float ), ( Float, Float ) )
moveTo state to =
  let
    currentPoint =
      getCurrentPoint state

    ( padding, xCoordinateSystem, yCoordinateSystem ) =
      fromMaybe
        ( error "Coordinate system undefined" )
        ( getFirst ( GraphicsState.coordinateSystem state ) )

    decodeCoordElement :: Format.Format -> EncodedDecimal.EncodedDecimal -> Float
    decodeCoordElement fmt EncodedDecimal.EncodedDecimal{ negative, digits } =
      let
        len =
          Format.integerPositions fmt + Format.decimalPositions fmt

        padded =
          case padding of
            Padding.PadLeading ->
              replicate ( len - length digits ) 0 <> digits

            Padding.PadTrailing ->
              take len ( digits ++ repeat 0 )

        ( intPart, dec ) =
          splitAt ( Format.integerPositions fmt ) padded

        sign =
          if negative then
            -1

          else
            1

      in
      toMM state
        ( read ( map intToDigit intPart <> "." <> map intToDigit dec ) * sign )

  in
  ( ( maybe
        ( fst currentPoint )
        ( decodeCoordElement xCoordinateSystem )
        ( Movement.x to )
    , maybe
        ( snd currentPoint )
        ( decodeCoordElement yCoordinateSystem )
        ( Movement.y to )
    )
  , ( maybe
        0
        ( decodeCoordElement xCoordinateSystem )
        ( Movement.i to )
    , maybe
        0
        (decodeCoordElement yCoordinateSystem )
        ( Movement.j to )
    )
  )


getCurrentPoint :: GraphicsState.GraphicsState m -> (Float, Float)
getCurrentPoint state =
  fromMaybe
    ( error "Current point undefined" )
    ( getLast ( GraphicsState.currentPoint state ) )


toMM :: Fractional a => GraphicsState.GraphicsState m -> a -> a
toMM state x =
  let
    unit =
      fromMaybe
        ( error "Units not set" )
        ( getFirst ( GraphicsState.unit state ) )

  in
  case unit of
    Unit.MM ->
      x

    Unit.IN ->
      x * 25.4


currentAperture
  :: HasCallStack
  => GraphicsState.GraphicsState m -> ApertureDefinition.ApertureDefinition
currentAperture state =
  let
    DCodeNumber.DCodeNumber aperture =
      fromMaybe
        ( error "No current aperture selected" )
        ( getLast ( GraphicsState.currentAperture state ) )

  in
  fromMaybe
      ( error ( "No definition for aperture D" <> show aperture ) )
      ( IntMap.lookup aperture ( GraphicsState.apertureDictionary state ) )


currentPolarity
  :: HasCallStack
  => GraphicsState.GraphicsState m -> Polarity.Polarity
currentPolarity state =
  fromMaybe
    ( error "Polarity not set" )
    ( getLast ( GraphicsState.polarity state ) )


currentInterpolationMode
  :: HasCallStack
  => GraphicsState.GraphicsState m -> InterpolationMode.InterpolationMode
currentInterpolationMode state =
  fromMaybe
    InterpolationMode.Linear
    ( getLast ( GraphicsState.interpolationMode state ) )


inRegion :: GraphicsState.GraphicsState m -> Bool
inRegion =
  fromMaybe False . getLast . GraphicsState.inRegion
