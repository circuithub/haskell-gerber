{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Gerber.Diagrams where

import Prelude

import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Except
import Control.Arrow (second)
import Data.Functor.Foldable hiding (project)
import Data.Gerber.Types
import Data.IntMap (IntMap)
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.List (zip)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Prelude hiding (Context, Name, trace, rotation, scaling, Linear, union, difference, at)
import Diagrams.TwoD.Path.Boolean
{-import qualified Diagrams.Prelude as Diagrams-}
import Linear.Epsilon
{-import Linear hiding (trace, rotate)-}

import Text.Show.Pretty (ppShow)


data AngleDirection
  = ClockWise
  | CounterClockWise
  deriving (Show, Eq, Ord)

data Interpolation
  = Linear
  | Circular AngleDirection
  deriving (Show, Eq, Ord)

data Quadrant
  = SingleQuadrant
  | MultipleQuadrant
  deriving (Show, Eq, Ord)

type Diagram' b = QDiagram b V2 Double Any
type Renderable' b = (Renderable (Path V2 Double) b, V b ~ V2, N b ~ Double)
type LineStyle' b = Diagram' b -> Diagram b

data Context b = Context
  { aperatureMap :: IntMap (Path V2 Double, Maybe (LineStyle' b))
  , macroMap :: Map Name MacroAt
  , coord :: Maybe (Point V2 Double)
  , aperature :: Maybe (Path V2 Double, Maybe (LineStyle' b))
  , interpolation :: Maybe Interpolation
  , quadrant :: Maybe Quadrant
  , polarity :: Polarity
  , mirroring :: Mirroring
  , rotation :: Angle Double
  , scaling :: Double
  , inRegion :: Bool
  , currentTrail :: Maybe (Located (Trail V2 Double))
  , currentGeoms :: [Path V2 Double]
  }

lAperatureMap :: Lens' (Context b) (IntMap (Path V2 Double, Maybe (LineStyle' b)))
lAperatureMap = lens aperatureMap (\c x -> c { aperatureMap = x })
lMacroMap :: Lens' (Context b) (Map Name MacroAt)
lMacroMap = lens macroMap (\c x -> c { macroMap = x })
lCoord :: Lens' (Context b) (Maybe (Point V2 Double))
lCoord = lens coord (\c x -> c {coord = x })
lAperature     :: Lens' (Context b) (Maybe (Path V2 Double, Maybe (LineStyle' b)))
lAperature = lens aperature (\c x -> c {aperature = x})
lInterpolation :: Lens' (Context b) (Maybe Interpolation)
lInterpolation = lens interpolation (\c x -> c {interpolation = x})
lQuadrant      :: Lens' (Context b) (Maybe Quadrant)
lQuadrant = lens quadrant (\c x -> c {quadrant = x})
lPolarity      :: Lens' (Context b) Polarity
lPolarity = lens polarity (\c x -> c {polarity = x})
lMirroring     :: Lens' (Context b) Mirroring
lMirroring = lens mirroring (\c x -> c {mirroring = x})
lRotation      :: Lens' (Context b) (Angle Double)
lRotation = lens rotation (\c x -> c {rotation = x})
lScaling       :: Lens' (Context b) Double
lScaling = lens scaling (\c x -> c {scaling = x})
lInRegion      :: Lens' (Context b) Bool
lInRegion = lens inRegion (\c x -> c {inRegion = x})
lCurrentTrail :: Lens' (Context b) (Maybe (Located (Trail V2 Double)))
lCurrentTrail = lens currentTrail (\c x -> c {currentTrail = x})
lCurrentGeoms :: Lens' (Context b) [Path V2 Double]
lCurrentGeoms = lens currentGeoms (\c x -> c {currentGeoms = x})

initialContext :: Context b
initialContext =
  Context
  { aperatureMap = IntMap.empty
  , macroMap = Map.empty
  , coord = mzero
  , aperature = mzero
  , interpolation = Just Linear -- mzero
  , quadrant = mzero
  , polarity = PolarityDark
  , mirroring = DontMirror
  , rotation = 0 @@ deg
  , scaling = 1
  , inRegion = False
  , currentTrail = mzero
  , currentGeoms = mzero
  }

drawDiagram
  :: (Renderable' b)
  => GerberFile -> Either String (Diagram' b)
drawDiagram g = do
  let
    isUnits c =
      case c of
        UnitsGb{} -> True
        _ -> False
        
    units =
      case filter isUnits g of
        ( UnitsGb u :_ ) ->
          u

        _ ->
          error "Unknown units" 
    
    factor =
      case units of
        Inches -> 25.4
        Milimeters -> 1

  diagram <-
    runIdentity
      (runExceptT
         (getDual . snd <$> runWriterT (void $ runStateT (drawDiagram' (toGerberFileF g)) initialContext)))

  return ( scale factor diagram )

drawDiagram' :: (MonadError String m,
                MonadWriter (Dual (Diagram' b)) m,
                MonadState (Context b) m,
                Renderable' b) => Fix GerberCommandF -> m ()
drawDiagram' = cata stepDiagram

stepDiagram
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  => GerberCommandF (m ()) -> m ()
stepDiagram (GerberFileF as) = sequence_ as
----
stepDiagram (AperatureDefF AperatureDef {..}) =
  do  ctx <- get
      d <-
        case aperatureDefType of
          StrokeAdt x ->  pure $ second Just (strokeAperature x)
          StandardAdt x -> pure $ (,Nothing) (standardAperature x)
          MacroAdt x -> (,Nothing) <$> macroAperature ctx x
      lAperatureMap %= IntMap.insert (getAperaturId aperatureDefId) d
stepDiagram (MacroF a@MacroAt {..}) =
  lMacroMap %= Map.insert macroAtTemplateName a
---
stepDiagram (CoordinateFormatF _) = pure ()
stepDiagram (UnitsF _) = pure ()
---
stepDiagram (BlockCommandF a) = a
---
stepDiagram (GrapicsStateF a) =
  case a of
    SetInterpolationLinear ->  lInterpolation  .= Just Linear
    SetInterpolationCircularClockWise ->  lInterpolation  .= Just (Circular ClockWise)
    SetInterpolationCircularCounterClockWise ->  lInterpolation  .= Just (Circular CounterClockWise)
    SetInterpolationCircularSingleQuadrant ->  lQuadrant  .= Just SingleQuadrant
    SetInterpolationCircularMultiQuadrant ->  lQuadrant  .= Just MultipleQuadrant
---
stepDiagram (InterpolateToF a) =
  use lInRegion >>= \x -> if x then interpolateToRegion a else interpolateToStroke a
stepDiagram (MoveToF a) =
  use lInRegion >>= \x -> if x then moveToRegion a else moveToStroke a
stepDiagram (FlashToF a) =
  use lInRegion >>= \x -> if x then throwError "Can't flash inside a region" else flashToRegion a
stepDiagram (RegionF as) =
  do lInRegion .= True
     sequence_ as
     lInRegion .= False
     closeRegion
     gs <- use lCurrentGeoms
     lCurrentGeoms .= mzero
     pol <- use lPolarity
     let fillStyle =
          case pol of
            PolarityDark -> fc black . lcA transparent . lwG 0
            PolarityClear -> fc white .  lcA transparent . lwG 0
     if (not . null) gs
      -- Use EvenOdd fill mode to trivially support cutins although it might be slower
      then tell . Dual . fillStyle . strokePath . union EvenOdd . mconcat $ gs
      else pure ()
stepDiagram (SetAperatureF SetAperature{..}) =
  use (lAperatureMap . to (IntMap.lookup $ getAperaturId setAperatureId)) >>= (lAperature .=)
stepDiagram (ObjectOptionsF a) =
  case a of
    LoadPolarity p -> lPolarity .= p
    LoadMirroring m -> lMirroring .= m
    LoadRotation d -> lRotation .= (d @@ deg)
    LoadScaling s -> lScaling .= s
stepDiagram (BlockAperatureF _) = throwError "BlockAperatureF - not yet implemented"
stepDiagram (StepRepeatF _) = throwError "StepRepeatF - not yet implemented"
stepDiagram (AttributeF _) = pure ()
stepDiagram (DeprecatedF _) = pure ()
stepDiagram (CommentF _) = pure ()

strokeAperature
  :: (Renderable' b)
  => StrokeAd -> (Path V2 Double, LineStyle' b)
strokeAperature (CircleStrokeAd d) =
  (circle (d / 2), lwG (d / 2) . lineCap LineCapRound . lineJoin LineJoinRound)
strokeAperature RectangleStrokeAd {..} =
  ( rect rectangleStrokeAdX rectangleStrokeAdY
  , lwG ((rectangleStrokeAdX + rectangleStrokeAdY) / 4) .
    lineCap LineCapSquare . lineJoin LineJoinMiter)

standardAperature :: StandardAd -> Path V2 Double
standardAperature CircleStandardAd {circleStandardAdDiameter=d,..} =
  makeHole (Just circleStandardAdHoleDiameter) (circle (d / 2))
standardAperature RectangleStandardAd {..} =
  makeHole (Just rectangleStandardAdHoleDiameter) (rect rectangleStandardAdX rectangleStandardAdY)
standardAperature ObroundStandardAd {..} =
  makeHole  obroundStandardAdHoleDiameter $
  case (obroundStandardAdX, obroundStandardAdY) of
    (x,y) | x < y ->
      union Winding (mconcat [translate (V2 0 ((x - y)/2)) (circle (x/2)), translate (V2 0 ((y - x)/2)) (circle (x/2)), rect x (y-x)])
    (x,y) | x > y ->
      union Winding (mconcat [translate (V2 ((y - x)/2) 0) (circle (y/2)), translate (V2 ((x - y)/2) 0) (circle (y/2)), rect (x-y) y])
    (x,_) -> circle (x/2)
standardAperature PolygonStandardAd {..} =
  makeHole polygonStandardAdHoleDiameter . maybe id (rotate . (@@ deg)) polygonStandardAdRotation $
  polygon
    (with & polyType .~
     PolyRegular polygonStandardAdVertices (polygonStandardAdDiamter / 2))

makeHole :: Maybe Double -> Path V2 Double -> Path V2 Double
makeHole Nothing a = a
makeHole (Just d) a = difference EvenOdd a (circle (d/2))

macroAperature
  :: (MonadError String m)
  => Context b -> MacroAd -> m (Path V2 Double)
macroAperature ctx MacroAd {..} =
  do m <-
       maybe
         (throwError $ "Could not find macro template with name " ++ show macroAdTempalateId)
         pure
         (ctx ^. (lMacroMap . at macroAdTempalateId))
     m' <- either throwError pure (evalMacro m macroAdModifiers)
     let eds = map (badExps . macroMpPath) m'
         badExps a@(Left _) = a
         badExps (Right (ExposureVar _,_)) = Left "MacroAperature - Error in processing macros got unfulfilled exposure"
         badExps a = a
     case (lefts eds, rights eds) of
      (es@(_:_), _) -> throwError (unlines es)
      (_, []) -> throwError "MacroAperature - Nothing to draw"
      (_, ds) -> maybe (throwError "MacroAperature - No exposures were on") pure $ foldl' cmb Nothing ds
  where
    cmb Nothing (ExposureOn, p') = Just p'
    cmb Nothing (ExposureOff, _) = Nothing
    cmb (Just p) (ExposureOn, p') = Just (union EvenOdd (p <> p'))
    cmb (Just p) (ExposureOff, p') = Just (difference EvenOdd p p')
    cmb _ (ExposureVar _, _) = error "macroAperature - Should never be here"


updateCurrentTrail :: Located (Trail V2 Double) -> Maybe (Located (Trail V2 Double)) -> Maybe (Located (Trail V2 Double))
updateCurrentTrail n mc = Just $ maybe n (mapLoc (<>unLoc n)) mc

interpolateToRegion
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  => InterpolateTo -> m ()
interpolateToRegion InterpolateTo {..} =
  do start_ <- use lCoord >>= maybe (throwError "Can't intepolate without a coordinate set") pure
     updateCoord getInterpolateTo
     end_ <- fromMaybe (error "impossible") <$> use lCoord
     interp <- use lInterpolation
     quad <- use lQuadrant
     case (interp, quad, getCircularOffset) of
       (Nothing, _, _) -> throwError "Can't interpolate with no interpolation mode specified"
       (Just Linear, _, _) -> lCurrentTrail %= updateCurrentTrail (fromVertices [start_, end_])
       (Just _, _, Nothing) -> throwError "Can't circular interpolate with no circular offset specified"
       (Just _, Nothing, _) -> throwError "Can't circular interpolate with no quadrant mode specified"
       (Just (Circular dir_), Just SingleQuadrant, Just circularOffset_) ->
         do a <- arcSingleQuad dir_ start_ end_ circularOffset_
            lCurrentTrail %= updateCurrentTrail a
       (Just (Circular dir_), Just MultipleQuadrant, Just circularOffset_) ->
        do a <- arcMultiQuad dir_ start_ end_ circularOffset_
           lCurrentTrail %= updateCurrentTrail a



moveToRegion
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  => MoveTo -> m ()
moveToRegion MoveTo{..} =
  do closeRegion
     updateCoord getMoveTo

closeRegion
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  =>  m ()
closeRegion = do
  mt <- use lCurrentTrail
  lCurrentTrail .= mzero
  case mt of
    Just t -> lCurrentGeoms %= ((pathFromLocTrail . mapLoc glueTrail $ t)  :)
    Nothing -> pure ()


interpolateToStroke
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  => InterpolateTo -> m ()
interpolateToStroke InterpolateTo {..} =
  do aperature_ <- use lAperature >>= maybe (throwError "Can't interpolate without an aperature set") pure
     pol <- use lPolarity
     lineStyle <-
       ((case pol of
         PolarityDark -> fc black
         PolarityClear -> fc white).) <$>
       case aperature_ of
         (_, Just l) -> pure l
         _ -> throwError "Standard circle or rectangle aperture needs to be set for stroking"
     start_ <- use lCoord >>= maybe (throwError "Can't intepolate without a coordinate set") pure
     updateCoord getInterpolateTo
     end_ <- fromMaybe (error "impossible") <$> use lCoord
     interp <- use lInterpolation
     quad <- use lQuadrant
     case (interp, quad, getCircularOffset) of
       (Nothing, _, _) -> throwError "Can't interpolate with no interpolation mode specified"
       (Just Linear, _, _) -> tell . Dual .  lineStyle . fromVertices $ [start_, end_]
       (Just _, _, Nothing) -> throwError "Can't circular interpolate with no circular offset specified"
       (Just _, Nothing, _) -> throwError "Can't circular interpolate with no quadrant mode specified"
       (Just (Circular dir_), Just SingleQuadrant, Just circularOffset_) -> tell . Dual . lineStyle =<< arcSingleQuad dir_ start_ end_ circularOffset_
       (Just (Circular dir_), Just MultipleQuadrant, Just circularOffset_) -> tell . Dual . lineStyle =<< arcMultiQuad dir_ start_ end_ circularOffset_

arcSingleQuad
  :: (MonadError String m, TrailLike t, V t ~ V2, N t ~ Double, Transformable t)
  => AngleDirection
  -> Point V2 Double
  -> Point V2 Double
  -> CircularOffset
  -> m t
arcSingleQuad dir_ start_ end_ CircularOffset {..} =
  let choices =
        map
          (makeArc id dir_ start_ end_)
          [ CircularOffset ox oy
          | ox <- [negate circularOfsetX, circularOfsetX]
          , oy <- [negate circularOfsetY, circularOfsetY] ]
      filter_ (a, _) = abs (a ^. deg) <= 90
  in case filter filter_ choices of
       [] -> throwError "No valid choices for arc"
       ((_, a):_) -> a

arcMultiQuad
  :: (MonadError String m, TrailLike t, V t ~ V2, N t ~ Double, Transformable t)
  => AngleDirection
  -> Point V2 Double
  -> Point V2 Double
  -> CircularOffset
  -> m t
arcMultiQuad = fmap (fmap (fmap (fmap snd))) (makeArc noZero)
  where
    noZero a | nearZero (a ^. rad) = 360 @@ deg
    noZero a = a

makeArc
  :: (MonadError String m, TrailLike t, V t ~ V2, N t ~ Double, Transformable t)
  => (Angle Double -> Angle Double)
  -> AngleDirection
  -> Point V2 Double
  -> Point V2 Double
  -> CircularOffset
  -> (Angle Double, m t)
makeArc angleCheck dir_ start_ end_ CircularOffset {..} =
  let center_ = start_ .+^ V2 circularOfsetX circularOfsetY
      toStart = start_ .-. center_
      toEnd = end_ .-. center_
      ax = normalize toStart
      ay = perp ax
      x = toEnd `dot` ax
      y = toEnd `dot` ay
      angleCCW = angleCheck $
        case (dir_, atan2A y x)
             -- -180 < a < 180
              of
          (CounterClockWise, a)
            | a < (0 @@ deg) -> a ^+^ (360 @@ deg)
          (ClockWise, a)
            | a > (0 @@ deg) -> a ^-^ (360 @@ deg)
          (_, a) -> a
  in if nearZero toStart
       then (angleCCW, throwError "Can't have center offset at start point")
       else ( angleCCW
            , pure . translate (center_ ^. _Point) . scale (norm toStart) $
              arc (direction toStart) angleCCW)

moveToStroke
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  => MoveTo -> m ()
moveToStroke MoveTo{..} = updateCoord getMoveTo

updateCoord
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  => CoordinateModify -> m ()
updateCoord CoordinateModify{..}=
  do  mc <- use lCoord
      case (mc, coordinateModifyX, coordinateModifyY) of
        (Nothing, Nothing, Nothing) -> pure ()
        (Nothing, Nothing, Just _) -> throwError "Can't only update X position if no coordinate is present"
        (Nothing, Just _, Nothing) -> throwError "Can't only update Y position if no coordinate is present"
        (Nothing, Just x, Just y) -> lCoord .= Just (P (V2 x y))
        _ ->
          do maybe (pure ()) (lCoord . _Just . _x .=) coordinateModifyX
             maybe (pure ()) (lCoord . _Just . _y .=) coordinateModifyY


flashToRegion
  :: (MonadError String m
     ,MonadWriter (Dual (Diagram' b)) m
     ,Renderable' b
     ,MonadState (Context b) m)
  => FlashTo -> m ()
flashToRegion FlashTo {..}=
  do updateCoord getFlashTo
     at_ <- use lCoord >>= maybe (throwError "Can't flash without a coordinate set") pure
     aperature_ <- ((fmap fst) <$> use lAperature) >>= maybe (throwError "Can't flash without an aperature set") pure
     pol <- use lPolarity
     let fillStyle =
          case pol of
            PolarityDark -> fc black . lcA transparent . lwG 0
            PolarityClear -> fc white .  lcA transparent . lwG 0
     tell . Dual . fillStyle . moveTo at_ . strokePath $ aperature_


data MacroPrimitive'
  = CircleMp' { circleMpExposure' :: Exposure
             , circleMpDiameter' :: Double
             , circleMpX' :: Double
             , circleMpY' :: Double
             , circleMpRotation' :: Maybe Double}
  | VectorLineMp' { vectorLineMpExposure' :: Exposure
                 , vectorLineMpWidth' :: Double
                 , vectorLineMpStartX' :: Double
                 , vectorLineMpStartY' :: Double
                 , vectorLineMpEndX' :: Double
                 , vectorLineMpEndY' :: Double
                 , vectorLineMpRotation' :: Maybe Double}
  | CenterLineMp' { centerLineMpExposure' :: Exposure
                 , centerLineMpWidth' :: Double
                 , centerLineMpHeight' :: Double
                 , centerLineMpX' :: Double
                 , centerLineMpY' :: Double
                 , centerLineMpRotation' :: Maybe Double}
  | OutlineMp' { outlineMpExposure' :: Exposure
              , outlineMpPoints' :: [(Double, Double)]
              , outlineMpRotation' :: Maybe Double}
  | PolygonMp' { polygonMpExposure' :: Exposure
              , polygonMpVertices' :: Int -- technically according to the spec any expression but assuming that is an error by ommision
              , polygonMpX' :: Double
              , polygonMpY' :: Double
              , polygonMpDiameter' :: Double
              , polygonMpRotation' :: Maybe Double}
  | MoireMp' { moireMpX' :: Double
            , moireMpY' :: Double
            , moireMpDiameter' :: Double
            , moireMpRingThickness' :: Double
            , moireMpRingGap' :: Double
            , moireMpMaxRingCount' :: Double
            , moireMpCrossHairThickness' :: Double
            , moireMpCrossHairLength' :: Double
            , moireMpRotation' :: Maybe Double}
  | ThermalMp' { thermalMpX' :: Double
              , thermalMpY' :: Double
              , thermalMpOuterDiameter' :: Double
              , thermalMpInnerDiameter' :: Double
              , thermalMpGapThickness' :: Double
              , thermalMpRotation' :: Maybe Double}
  deriving Show

maybeRotateDeg
  :: (N t ~ Double, V t ~ V2, Transformable t)
  => Maybe Double -> t -> t
maybeRotateDeg = maybe id (rotate . (@@ deg))


evalMacro :: MacroAt -> [Double] ->  Either String [MacroPrimitive']
evalMacro MacroAt {..} vs = runIdentity (runExceptT (fst <$> (runStateT top initS)))
  where
    top :: (MonadState (IntMap Double) m, MonadError String m) => m [MacroPrimitive']
    top = catMaybes <$> traverse eval macroAtConent
    initS :: IntMap Double
    initS = IntMap.fromList (zip [1..] vs)
    eval :: (MonadState (IntMap Double) m, MonadError String m) => MacroContent -> m (Maybe MacroPrimitive')
    eval (VariableMc MacroVariable {..}) =
      do e <- evalExpr macroVariableExpression
         modify (IntMap.insert macroVariableName e)
         pure Nothing
    eval (PrimitiveMc CommentMp {}) = pure Nothing
    eval (PrimitiveMc CircleMp {..}) =
      Just <$>
      (CircleMp' <$> evalExposure circleMpExposure <*> evalExpr circleMpDiameter <*>
       evalExpr circleMpX <*>
       evalExpr circleMpY <*>
       mevalExpr circleMpRotation)
    eval (PrimitiveMc VectorLineMp {..}) =
      Just <$>
      (VectorLineMp' <$> evalExposure vectorLineMpExposure <*> evalExpr vectorLineMpWidth <*>
       evalExpr vectorLineMpStartX <*>
       evalExpr vectorLineMpStartY <*>
       evalExpr vectorLineMpEndX <*>
       evalExpr vectorLineMpEndY <*>
       mevalExpr vectorLineMpRotation)
    eval (PrimitiveMc CenterLineMp {..}) =
      Just <$>
      (CenterLineMp' <$> evalExposure centerLineMpExposure <*> evalExpr centerLineMpWidth <*>
       evalExpr centerLineMpHeight <*>
       evalExpr centerLineMpX <*>
       evalExpr centerLineMpY <*>
       mevalExpr centerLineMpRotation)
    eval (PrimitiveMc OutlineMp {..}) =
      Just <$>
      (OutlineMp' <$> evalExposure outlineMpExposure <*> traverse (both evalExpr) outlineMpPoints <*>
       mevalExpr outlineMpRotation)
    eval (PrimitiveMc PolygonMp {..}) =
      Just <$>
      (PolygonMp' <$> evalExposure polygonMpExposure <*> pure polygonMpVertices <*>
       evalExpr polygonMpX <*>
       evalExpr polygonMpY <*>
       evalExpr polygonMpDiameter <*>
       mevalExpr polygonMpRotation)
    eval (PrimitiveMc MoireMp {}) = throwError "Moire pattern not supported yet"
    eval (PrimitiveMc ThermalMp {}) = throwError "Thermal pattern not supported yet"
    evalExposure :: (MonadState (IntMap Double) m, MonadError String m) => Exposure -> m Exposure
    evalExposure (ExposureVar i) =
      do e <- use (at i) >>= maybe (throwError $ "Failed to lookup variable " ++ show i) pure
         case round(e) of
            (0 :: Int) -> pure ExposureOff
            (1 :: Int) -> pure ExposureOn
            _ -> throwError ("Exposure should evaluate to lookup to 0 or 1")
    evalExposure e = pure e
    evalExpr :: (MonadState (IntMap Double) m, MonadError String m) => MacroModifier -> m Double
    evalExpr = cata evalExprF
    evalExprF :: (MonadState (IntMap Double) m, MonadError String m) => MacroModifierF (m Double) -> m Double
    evalExprF (ConstantMmF d) = pure d
    evalExprF (VarMmF i) = use (at i) >>= maybe (throwError $ "Failed to lookup variable " ++ show i) pure
    evalExprF (NegMmF a) = negate <$> a
    evalExprF (AddMmF a b) = (+) <$> a <*> b
    evalExprF (SubMmF a b) = (-) <$> a <*> b
    evalExprF (MulMmF a b) = (*) <$> a <*> b
    evalExprF (DivMmF a b) = (/) <$> a <*> b
    evalExprF (ParenMmF a) = a
    mevalExpr :: (MonadState (IntMap Double) m, MonadError String m) => Maybe MacroModifier -> m (Maybe Double)
    mevalExpr Nothing = pure Nothing
    mevalExpr (Just e) = Just <$> evalExpr e


macroMpPath :: MacroPrimitive' -> Either String (Exposure, Path V2 Double)
macroMpPath x@CircleMp' {..} =
  Right
    ( circleMpExposure'
    , maybeRotateDeg circleMpRotation' .
      translate (V2 circleMpX' circleMpY') . circle $
      circleMpDiameter' / 2)
macroMpPath VectorLineMp' {..} =
  let vs = V2 vectorLineMpStartX' vectorLineMpStartY'
      ve = V2 vectorLineMpEndX' vectorLineMpEndY'
      vc = (vs + ve) ^* 0.5
      vd@(V2 dx dy) = ve ^-^ vs
      a = atan2A dy dx
  in Right
       ( vectorLineMpExposure'
       , maybeRotateDeg vectorLineMpRotation' . translate vc . rotate a $
         rect vectorLineMpWidth' (norm vd))
macroMpPath CenterLineMp' {..} =
  let vc = V2 centerLineMpX' centerLineMpY'
  in Right
       ( centerLineMpExposure'
       , maybeRotateDeg centerLineMpRotation' . translate vc $
         rect centerLineMpWidth' centerLineMpHeight')
macroMpPath OutlineMp' {..} =
   Right
       ( outlineMpExposure'
       , maybeRotateDeg outlineMpRotation' .
         pathFromLocTrail . mapLoc glueTrail . fromVertices . map p2 $
         outlineMpPoints')
macroMpPath PolygonMp' {..} =
  let vc = V2 polygonMpX' polygonMpY'
  in Right
       ( polygonMpExposure'
       , maybeRotateDeg polygonMpRotation' . pathFromLocTrail . translate vc $
         polygon
           (with & polyType .~
            PolyRegular polygonMpVertices' (polygonMpDiameter' / 2)))
macroMpPath MoireMp' {} = Left "Moire pattern not supported yet"
macroMpPath ThermalMp' {} = Left "Thermal pattern not supported yet"
