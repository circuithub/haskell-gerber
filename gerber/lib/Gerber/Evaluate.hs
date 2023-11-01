{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Gerber.Evaluate (
  Evaluator (..),
  evaluate,
) where

-- base
import Data.Char (intToDigit)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Dual (..), First (..), getLast)
import GHC.Stack (HasCallStack)

-- containers
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

-- foldl
import qualified Control.Foldl as Fold

-- gerber
import qualified Gerber.ApertureDefinition as ApertureDefinition
import qualified Gerber.Command as Command
import qualified Gerber.DCodeNumber as DCodeNumber
import qualified Gerber.EncodedDecimal as EncodedDecimal
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.Evaluate.GraphicsState as GraphicsState
import qualified Gerber.Evaluate.GraphicsState.ApertureEntry as ApertureEntry
import qualified Gerber.Evaluate.GraphicsState.InterpolationMode as InterpolationMode
import qualified Gerber.Evaluate.GraphicsState.StackStream as StackStream
import qualified Gerber.Format as Format
import qualified Gerber.MacroDefinition as MacroDefinition
import qualified Gerber.Mirroring as Mirroring
import qualified Gerber.Movement as Movement
import qualified Gerber.Padding as Padding
import qualified Gerber.Polarity as Polarity
import qualified Gerber.StepRepeat as StepRepeat
import qualified Gerber.Unit as Unit

-- monoid-extras
import Data.Monoid.Deletable (deleteR, toDeletable, unDelete)

-- mtl
import Control.Monad.State hiding (state)

-- text
import Data.Text (Text)
import qualified Data.Text as Text


data Evaluator m = Evaluator
  { line ::
      Polarity.Polarity ->
      ApertureDefinition.ApertureDefinition ->
      (Float, Float) ->
      (Float, Float) ->
      m
  , flash ::
      Polarity.Polarity ->
      ApertureDefinition.ApertureDefinition ->
      m
  , flashMacro ::
      Polarity.Polarity ->
      [MacroDefinition.Primitive MacroDefinition.Exposure Float] ->
      m
  , fillRegion ::
      Polarity.Polarity ->
      (Float, Float) ->
      [Edge.Edge] ->
      m
  , arc ::
      Polarity.Polarity ->
      ApertureDefinition.ApertureDefinition ->
      (Float, Float) ->
      (Float, Float) ->
      (Float, Float) ->
      m
  , translate ::
      Float ->
      Float ->
      m ->
      m
  , mirror :: Mirroring.Mirroring -> m -> m
  , rotate :: Float -> m -> m
  , scale :: Float -> m -> m
  }


evaluate :: Monoid m => Evaluator m -> Fold.Fold Command.Command m
evaluate = evaluate' GraphicsState.initialGraphicsState


evaluate' :: Monoid m => GraphicsState.GraphicsState m -> Evaluator m -> Fold.Fold Command.Command m
evaluate' initialStatue evaluator =
  Fold.Fold
    ( \(drawing, state) command ->
        let
          (d, s) =
            step evaluator state command
         in
          (d <> drawing, state <> s)
    )
    (mempty, initialStatue)
    fst


step ::
  forall m.
  Monoid m =>
  Evaluator m ->
  GraphicsState.GraphicsState m ->
  Command.Command ->
  (m, GraphicsState.GraphicsState m)
step evaluator state
  | not (StackStream.null (GraphicsState.blockApertureStack state)) = \command ->
      case command of
        Command.AB a ->
          abStart a
        Command.AB_End ->
          abEnd
        _ ->
          ( mempty
          , mempty{GraphicsState.blockApertureStack = StackStream.add command}
          )
  | otherwise = \case
      Command.FS padding xfmt yfmt ->
        ( mempty
        , mempty{GraphicsState.coordinateSystem = pure (padding, xfmt, yfmt)}
        )
      Command.AD (DCodeNumber.DCodeNumber n) def ->
        let
          scaleRectParams params =
            params
              { ApertureDefinition.width =
                  toMM state (ApertureDefinition.width params)
              , ApertureDefinition.height =
                  toMM state (ApertureDefinition.height params)
              }

          defInMM =
            case def of
              ApertureDefinition.Circle params ->
                ApertureDefinition.Circle
                  params
                    { ApertureDefinition.diameter =
                        toMM state (ApertureDefinition.diameter params)
                    }
              ApertureDefinition.Rectangle params ->
                ApertureDefinition.Rectangle (scaleRectParams params)
              ApertureDefinition.Obround params ->
                ApertureDefinition.Obround (scaleRectParams params)
              ApertureDefinition.Polygon params ->
                ApertureDefinition.Polygon
                  params
                    { ApertureDefinition.outerDiameter =
                        toMM state (ApertureDefinition.outerDiameter params)
                    , ApertureDefinition.rotation =
                        toMM state <$> ApertureDefinition.rotation params
                    , ApertureDefinition.polygonHoleDiameter =
                        toMM state <$> ApertureDefinition.polygonHoleDiameter params
                    }
         in
          ( mempty
          , mempty
              { GraphicsState.apertureDictionary = IntMap.singleton n (ApertureEntry.BasicAperture defInMM)
              }
          )
      Command.MacroAD (DCodeNumber.DCodeNumber n) name arguments ->
        let
          prim = lookupEvalMacroInMM state name arguments
         in
          ( mempty
          , mempty
              { GraphicsState.apertureDictionary = IntMap.singleton n (ApertureEntry.MacroAperture prim)
              }
          )
      Command.D n ->
        ( mempty
        , mempty{GraphicsState.currentAperture = pure n}
        )
      Command.G04 _ ->
        mempty
      Command.MO units ->
        ( mempty
        , mempty{GraphicsState.unit = pure units}
        )
      Command.LP polarity ->
        (mempty, mempty{GraphicsState.polarity = pure polarity})
      Command.D01 to
        | inRegion state ->
            let
              (newPoint, offset) =
                moveTo state to
             in
              ( mempty
              , mempty
                  { GraphicsState.currentContour =
                      toDeletable
                        ( mempty
                        , let
                            center =
                              let
                                (dx, dy) =
                                  offset
                               in
                                (fst currentPoint + dx, snd currentPoint + dy)

                            currentPoint =
                              getCurrentPoint state
                           in
                            pure $ case currentInterpolationMode state of
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
                        )
                  , GraphicsState.currentPoint =
                      pure newPoint
                  }
              )
      Command.D01 to | otherwise -> drawToImageOrStepRepeat $
        case currentAperture state of
          ApertureEntry.MacroAperture _ ->
            mempty
          ApertureEntry.BlockAperture _ ->
            mempty
          ApertureEntry.BasicAperture apertureDefinition ->
            let
              interp =
                currentInterpolationMode state

              (newPoint, offset) =
                moveTo state to

              polarity =
                currentPolarity state

              currentPoint =
                getCurrentPoint state

              center =
                let
                  (dx, dy) =
                    offset
                 in
                  (fst currentPoint + dx, snd currentPoint + dy)
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
                    arc evaluator polarity apertureDefinition newPoint center currentPoint
                  InterpolationMode.CircularCCW ->
                    arc evaluator polarity apertureDefinition currentPoint center newPoint
              , mempty{GraphicsState.currentPoint = pure newPoint}
              )
      Command.D02 to
        | inRegion state ->
            let
              (newPoint, _) =
                moveTo state to

              polarity =
                currentPolarity state
             in
              ( case unDelete (GraphicsState.currentContour state) of
                  (First (Just start), edges) ->
                    fillRegion evaluator polarity start (toList edges)
                  _ ->
                    error "Region finished without any edges"
              , mempty
                  { GraphicsState.currentContour =
                      deleteR <> toDeletable (pure newPoint, mempty)
                  , GraphicsState.currentPoint =
                      pure newPoint
                  }
              )
      Command.D02 to ->
        drawToImageOrStepRepeat $
          let
            (newPoint, _) =
              moveTo state to
           in
            ( mempty
            , mempty{GraphicsState.currentPoint = pure newPoint}
            )
      Command.D03 to ->
        drawToImageOrStepRepeat $
          let
            (newPoint, _) =
              moveTo state to

            apertureDefinition =
              currentAperture state

            polarity =
              currentPolarity state

            transform = applyTranslation . applyRotation . applyMirror . applyScale
              where
                applyScale = maybe id (scale evaluator) (getLast (GraphicsState.scale state))

                applyMirror = maybe id (mirror evaluator) (getLast (GraphicsState.mirror state))

                applyRotation = maybe id (rotate evaluator) (getLast (GraphicsState.rotate state))

                applyTranslation = uncurry (translate evaluator) newPoint

            drawing' =
              transform $
                case apertureDefinition of
                  ApertureEntry.BasicAperture a ->
                    flash evaluator polarity a
                  ApertureEntry.MacroAperture a ->
                    flashMacro evaluator polarity a
                  ApertureEntry.BlockAperture commands ->
                    -- From the specification section 4.6.1
                    --
                    -- If the polarity is dark (LPD) when the block is flashed then the block aperture is inserted as is. If
                    -- the polarity is clear (LPC) then the polarity of all objects in the block is toggled (clear becomes
                    -- dark, and dark becomes clear). This toggle propagates through all nesting levels.
                    let commands'
                          | currentPolarity_ == Just Polarity.Clear = map flipPolarity commands
                          | otherwise = commands

                        currentPolarity_ = getLast (GraphicsState.polarity state)

                        flipPolarity = \case
                          Command.LP Polarity.Dark ->
                            Command.LP Polarity.Clear
                          Command.LP Polarity.Clear ->
                            Command.LP Polarity.Dark
                          a -> a
                     in Fold.fold (evaluate' state evaluator) commands'
           in
            ( drawing'
            , mempty{GraphicsState.currentPoint = pure newPoint}
            )
      Command.G36 ->
        ( mempty
        , mempty
            { GraphicsState.inRegion =
                pure True
            , GraphicsState.currentContour =
                deleteR <> toDeletable (pure (getCurrentPoint state), mempty)
            }
        )
      Command.G37{} ->
        drawToImageOrStepRepeat $
          let
            polarity =
              currentPolarity state
           in
            ( case unDelete (GraphicsState.currentContour state) of
                (First (Just start), edges) ->
                  fillRegion evaluator polarity start (toList edges)
                _ ->
                  mempty
            , mempty
                { GraphicsState.inRegion = pure False
                }
            )
      Command.IP{} ->
        mempty
      Command.G74{} ->
        mempty
      Command.G75{} ->
        mempty
      Command.G01 ->
        ( mempty
        , mempty
            { GraphicsState.interpolationMode = pure InterpolationMode.Linear
            }
        )
      Command.G02 ->
        ( mempty
        , mempty
            { GraphicsState.interpolationMode = pure InterpolationMode.CircularCW
            }
        )
      Command.G03 ->
        ( mempty
        , mempty
            { GraphicsState.interpolationMode = pure InterpolationMode.CircularCCW
            }
        )
      Command.G71 ->
        (mempty, mempty{GraphicsState.unit = pure Unit.MM})
      Command.OF (Just 0) (Just 0) ->
        mempty
      Command.AM name definition ->
        ( mempty
        , (mempty @(GraphicsState.GraphicsState m))
            { GraphicsState.macroDictionary = Map.singleton name definition
            }
        )
      Command.SR movement ->
        flushSR
          ( mempty
          , (mempty @(GraphicsState.GraphicsState m))
              { GraphicsState.stepRepeat = deleteR <> toDeletable (pure movement, mempty)
              }
          )
      Command.SR_End ->
        flushSR
          ( mempty
          , (mempty @(GraphicsState.GraphicsState m))
              { GraphicsState.stepRepeat = deleteR <> toDeletable (mempty, mempty)
              }
          )
      Command.M02 ->
        flushSR mempty
      Command.SF ->
        mempty
      Command.MI ->
        mempty
      Command.AB a ->
        abStart a
      Command.AB_End ->
        abEnd
      Command.LM a ->
        ( mempty
        , mempty{GraphicsState.mirror = pure a}
        )
      Command.LS a ->
        ( mempty
        , mempty{GraphicsState.scale = pure a}
        )
      Command.LR a ->
        ( mempty
        , mempty{GraphicsState.rotate = pure a}
        )
      Command.TF _ ->
        -- Non graphics affecting meta information
        mempty
      Command.TA _ _ ->
        -- Non graphics affecting meta information
        mempty
      Command.TO _ _ ->
        -- Non graphics affecting meta information
        mempty
      Command.TD _ ->
        -- TODO: This command clears out data from the aperture and object dictionaries
        -- Non graphics affecting meta information
        mempty
      Command.LN _ ->
        -- Non graphics affecting meta information
        mempty
      Command.OF _ _ ->
        -- From the specification section 7.1.8
        error "Unsupported command. The OF command is deprecated since revision I1 from December 2012."
  where
    abStart a =
      ( mempty
      , mempty{GraphicsState.blockApertureStack = StackStream.push a}
      )

    abEnd =
      case StackStream.top (GraphicsState.blockApertureStack state) of
        Nothing ->
          error "unbalanced aperture block start and end"
        Just (DCodeNumber.DCodeNumber apertureId, commands) ->
          ( mempty
          , mempty
              { GraphicsState.apertureDictionary = IntMap.singleton apertureId (ApertureEntry.BlockAperture commands)
              , GraphicsState.blockApertureStack = StackStream.pop
              }
          )

    flushSR =
      let
        close =
          case unDelete (GraphicsState.stepRepeat state) of
            (First (Just movement), Dual drawing) ->
              let
                StepRepeat.StepRepeat{xRepeats, yRepeats, xStep, yStep} =
                  movement
               in
                ( mconcat
                    ( do
                        xi <-
                          [1 .. xRepeats]

                        yi <-
                          [1 .. yRepeats]

                        return
                          ( translate
                              evaluator
                              (fromIntegral (xi - 1) * toMM state xStep)
                              (fromIntegral (yi - 1) * toMM state yStep)
                              drawing
                          )
                    )
                , mempty
                )
            _ ->
              mempty
       in
        mappend close

    drawToImageOrStepRepeat (drawing, stateChange) =
      case unDelete (GraphicsState.stepRepeat state) of
        (First Nothing, _) ->
          (drawing, stateChange)
        (_, _) ->
          ( mempty
          , stateChange <> (mempty @(GraphicsState.GraphicsState m)){GraphicsState.stepRepeat = toDeletable (mempty, Dual drawing)}
          )


moveTo ::
  GraphicsState.GraphicsState m ->
  Movement.Movement ->
  ((Float, Float), (Float, Float))
moveTo state to =
  let
    currentPoint =
      getCurrentPoint state

    (padding, xCoordinateSystem, yCoordinateSystem) =
      fromMaybe
        (error "Coordinate system undefined")
        (getFirst (GraphicsState.coordinateSystem state))

    decodeCoordElement :: Format.Format -> EncodedDecimal.EncodedDecimal -> Float
    decodeCoordElement fmt EncodedDecimal.EncodedDecimal{negative, digits} =
      let
        len =
          Format.integerPositions fmt + Format.decimalPositions fmt

        padded =
          case padding of
            Padding.PadLeading ->
              replicate (len - length digits) 0 <> digits
            Padding.PadTrailing ->
              take len (digits ++ repeat 0)

        (intPart, dec) =
          splitAt (length padded - Format.decimalPositions fmt) padded

        sign =
          if negative
            then -1
            else 1
       in
        toMM
          state
          (read (map intToDigit intPart <> "." <> map intToDigit dec) * sign)
   in
    (
      ( maybe
          (fst currentPoint)
          (decodeCoordElement xCoordinateSystem)
          (Movement.x to)
      , maybe
          (snd currentPoint)
          (decodeCoordElement yCoordinateSystem)
          (Movement.y to)
      )
    ,
      ( maybe
          0
          (decodeCoordElement xCoordinateSystem)
          (Movement.i to)
      , maybe
          0
          (decodeCoordElement yCoordinateSystem)
          (Movement.j to)
      )
    )


getCurrentPoint :: GraphicsState.GraphicsState m -> (Float, Float)
getCurrentPoint state =
  fromMaybe
    (error "Current point undefined")
    (getLast (GraphicsState.currentPoint state))


toMM :: Fractional a => GraphicsState.GraphicsState m -> a -> a
toMM state x =
  let
    unit =
      fromMaybe
        (error "Units not set")
        (getFirst (GraphicsState.unit state))
   in
    case unit of
      Unit.MM ->
        x
      Unit.IN ->
        x * 25.4


lookupEvalMacroInMM :: HasCallStack => GraphicsState.GraphicsState m -> Text -> [Float] -> [MacroDefinition.Primitive MacroDefinition.Exposure Float]
lookupEvalMacroInMM state name arguments = catMaybes $ evalState (traverse go content) initialState
  where
    name' = Text.unpack name

    content :: [MacroDefinition.Definition MacroDefinition.Modifier MacroDefinition.Modifier]
    content =
      fromMaybe
        (error ("Macro definition not found: " ++ name'))
        (Map.lookup name (GraphicsState.macroDictionary state))

    initialState :: IntMap.IntMap Float
    initialState = IntMap.fromList $ zip [1 ..] arguments

    go ::
      HasCallStack =>
      MacroDefinition.Definition MacroDefinition.Modifier MacroDefinition.Modifier ->
      State (IntMap.IntMap Float) (Maybe (MacroDefinition.Primitive MacroDefinition.Exposure Float))
    go = \case
      MacroDefinition.Variable variableName expression -> do
        v <- evalExpresion expression
        modify (IntMap.insert variableName v)
        pure Nothing
      MacroDefinition.Comment _ ->
        pure Nothing
      MacroDefinition.Primitive a ->
        Just <$> (traverse evalExpresion a >>= determineExposure . coordinatesToMM)
      MacroDefinition.InvalidDefinition _ _ ->
        pure Nothing
      where
        determineExposure ::
          HasCallStack =>
          MacroDefinition.Primitive MacroDefinition.Modifier Float ->
          State (IntMap.IntMap Float) (MacroDefinition.Primitive MacroDefinition.Exposure Float)
        determineExposure = \case
          MacroDefinition.Circle a ->
            (\exposure -> MacroDefinition.Circle a{MacroDefinition.circleExposure = exposure})
              <$> evalExposure (MacroDefinition.circleExposure a)
          MacroDefinition.VectorLine a ->
            (\exposure -> MacroDefinition.VectorLine a{MacroDefinition.vectorLineExposure = exposure})
              <$> evalExposure (MacroDefinition.vectorLineExposure a)
          MacroDefinition.CenterLine a ->
            (\exposure -> MacroDefinition.CenterLine a{MacroDefinition.centerLineExposure = exposure})
              <$> evalExposure (MacroDefinition.centerLineExposure a)
          MacroDefinition.Outline a ->
            (\exposure -> MacroDefinition.Outline a{MacroDefinition.outlineExposure = exposure})
              <$> evalExposure (MacroDefinition.outlineExposure a)
          MacroDefinition.Polygon a ->
            (\exposure -> MacroDefinition.Polygon a{MacroDefinition.polygonExposure = exposure})
              <$> evalExposure (MacroDefinition.polygonExposure a)
          MacroDefinition.Moire a ->
            pure (MacroDefinition.Moire a)
          MacroDefinition.Thermal a ->
            pure (MacroDefinition.Thermal a)
          where
            evalExposure = pure . toExposure <=< evalExpresion
              where
                toExposure x = if x >= 1 then MacroDefinition.ExposureOn else MacroDefinition.ExposureOff

        coordinatesToMM :: MacroDefinition.Primitive a Float -> MacroDefinition.Primitive a Float
        coordinatesToMM = \case
          MacroDefinition.Circle a ->
            MacroDefinition.Circle (toMM state <$> a){MacroDefinition.circleRotation = MacroDefinition.circleRotation a}
          MacroDefinition.VectorLine a ->
            MacroDefinition.VectorLine (toMM state <$> a){MacroDefinition.vectorLineRotation = MacroDefinition.vectorLineRotation a}
          MacroDefinition.CenterLine a ->
            MacroDefinition.CenterLine (toMM state <$> a){MacroDefinition.centerLineRotation = MacroDefinition.centerLineRotation a}
          MacroDefinition.Outline a ->
            MacroDefinition.Outline (toMM state <$> a){MacroDefinition.outlineRotation = MacroDefinition.outlineRotation a}
          MacroDefinition.Polygon a ->
            MacroDefinition.Polygon (toMM state <$> a){MacroDefinition.polygonRotation = MacroDefinition.polygonRotation a}
          MacroDefinition.Moire a ->
            MacroDefinition.Moire (toMM state <$> a){MacroDefinition.moireRotation = MacroDefinition.moireRotation a}
          MacroDefinition.Thermal a ->
            MacroDefinition.Thermal (toMM state <$> a){MacroDefinition.thermalRotation = MacroDefinition.thermalRotation a}

        evalExpresion :: HasCallStack => MacroDefinition.Modifier -> State (IntMap.IntMap Float) Float
        evalExpresion = \case
          MacroDefinition.Decimal a ->
            pure a
          MacroDefinition.VariableReference i ->
            gets (fromMaybe (error $ "Variable " ++ show i ++ " not found for macro " ++ name') . IntMap.lookup i)
          MacroDefinition.Parentheses a ->
            evalExpresion a
          MacroDefinition.UnaryPlus a ->
            evalExpresion a
          MacroDefinition.UnaryMinus a ->
            negate <$> evalExpresion a
          MacroDefinition.Plus a b ->
            (+) <$> evalExpresion a <*> evalExpresion b
          MacroDefinition.Minus a b ->
            (-) <$> evalExpresion a <*> evalExpresion b
          MacroDefinition.Multiply a b ->
            (*) <$> evalExpresion a <*> evalExpresion b
          MacroDefinition.Divide a b ->
            (/) <$> evalExpresion a <*> evalExpresion b


currentAperture ::
  HasCallStack =>
  GraphicsState.GraphicsState m ->
  ApertureEntry.ApertureEntry
currentAperture state =
  let
    DCodeNumber.DCodeNumber aperture =
      fromMaybe
        (error "No current aperture selected")
        (getLast (GraphicsState.currentAperture state))
   in
    fromMaybe
      (error ("No definition for aperture D" <> show aperture))
      (IntMap.lookup aperture (GraphicsState.apertureDictionary state))


currentPolarity ::
  HasCallStack =>
  GraphicsState.GraphicsState m ->
  Polarity.Polarity
currentPolarity state =
  fromMaybe
    (error "Polarity not set")
    (getLast (GraphicsState.polarity state))


currentInterpolationMode ::
  GraphicsState.GraphicsState m -> InterpolationMode.InterpolationMode
currentInterpolationMode state =
  fromMaybe
    InterpolationMode.Linear
    (getLast (GraphicsState.interpolationMode state))


inRegion :: GraphicsState.GraphicsState m -> Bool
inRegion =
  fromMaybe False . getLast . GraphicsState.inRegion
