{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Gerber.Diagrams (
  gerberToDiagram,
  gerberToDiagramCustom,
  Config (..),
  defaultConfig,
) where

-- base
import Data.List (foldl', unfoldr)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Typeable (Typeable)

-- diagrams-contrib
import qualified Diagrams.TwoD.Path.Boolean as Diagrams.Boolean

-- diagrams-lib
import Diagrams (N, V)
import qualified Diagrams.Path as Diagrams
import qualified Diagrams.Prelude as Diagrams

-- gerber
import qualified Gerber.ApertureDefinition as ApertureDefinition
import qualified Gerber.Evaluate as Gerber
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.MacroDefinition as MacroDefinition
import qualified Gerber.Mirroring as Mirroring
import qualified Gerber.Polarity as Polarity

-- linear
import Linear (Epsilon (nearZero), V2 (..), distance, (^+^), (^-^))
import Linear.Affine (Point (..), (.-.))


gerberToDiagram ::
  Diagrams.Renderable (Diagrams.Path Diagrams.V2 Double) b =>
  Gerber.Evaluator (Diagrams.QDiagram b Diagrams.V2 Double Diagrams.Any)
gerberToDiagram = gerberToDiagramCustom defaultConfig


-- diagrams crashes if the scale factor is zero. But scaling by
-- zero is really just an empty image, so we try and detect it and
-- return mempty.
safeCircle :: forall t n. (Diagrams.TrailLike t, V t ~ V2, N t ~ n, Diagrams.Transformable t, Monoid t, Epsilon n) => n -> t
safeCircle r
  | nearZero r = mempty
  | otherwise = Diagrams.circle r


gerberToDiagramCustom ::
  Diagrams.Renderable (Diagrams.Path Diagrams.V2 Double) b =>
  Config ->
  Gerber.Evaluator (Diagrams.QDiagram b Diagrams.V2 Double Diagrams.Any)
gerberToDiagramCustom config =
  Gerber.Evaluator
    { Gerber.arc =
        \polarity aperture currentPoint center end ->
          let
            arc =
              let
                toStart =
                  Diagrams.p2 currentPoint .-. Diagrams.p2 center

                toEnd =
                  Diagrams.p2 end .-. Diagrams.p2 center
               in
                -- diagrams crashes if the scale factor is zero. But scaling by
                -- zero is really just an empty image, so we try and detect it and
                -- return mempty.
                if not (Linear.nearZero (Diagrams.norm toStart))
                  then
                    Diagrams.translate
                      (realToFrac <$> Diagrams.r2 center)
                      ( Diagrams.scale
                          (realToFrac (Diagrams.norm toStart))
                          ( if all Linear.nearZero (toStart .-. toEnd)
                              then
                                Diagrams.arc'
                                  1
                                  (realToFrac <$> Diagrams.direction toStart)
                                  (1 Diagrams.@@ Diagrams.turn)
                              else
                                Diagrams.arcCCW
                                  (realToFrac <$> Diagrams.direction toStart)
                                  (realToFrac <$> Diagrams.direction toEnd)
                          )
                      )
                  else mempty
           in
            withPolarity config polarity (strokeAperture aperture arc)
    , Gerber.line =
        \polarity aperture (x1, y1) (x2, y2) ->
          let
            trail =
              Diagrams.strokeLocTrail
                ( Diagrams.P (realToFrac <$> Diagrams.V2 x1 y1)
                    Diagrams.~~ Diagrams.P (realToFrac <$> Diagrams.V2 x2 y2)
                )
           in
            withPolarity config polarity (strokeAperture aperture trail)
    , Gerber.flash =
        let circleFromDiameter = safeCircle . realToFrac . (/ 2)

            addHole solid = Diagrams.stroke . maybe solid withHole
              where
                withHole = Diagrams.Boolean.difference Diagrams.EvenOdd solid . circleFromDiameter
         in \polarity aperture ->
              withPolarity
                config
                polarity
                ( Diagrams.lw Diagrams.none $
                    case aperture of
                      ApertureDefinition.Circle params ->
                        addHole
                          (circleFromDiameter . ApertureDefinition.diameter $ params)
                          (ApertureDefinition.circleHoleDiameter params)
                      ApertureDefinition.Rectangle params ->
                        addHole
                          ( Diagrams.rect
                              (realToFrac $ ApertureDefinition.width params)
                              (realToFrac $ ApertureDefinition.height params)
                          )
                          (ApertureDefinition.rectangleHoleDiameter params)
                      ApertureDefinition.Obround params ->
                        addHole
                          ( Diagrams.roundedRect
                              (realToFrac $ ApertureDefinition.width params)
                              (realToFrac $ ApertureDefinition.height params)
                              ( realToFrac
                                  ( max
                                      (ApertureDefinition.width params)
                                      (ApertureDefinition.height params)
                                  )
                              )
                          )
                          (ApertureDefinition.rectangleHoleDiameter params)
                      ApertureDefinition.Polygon params ->
                        maybe
                          id
                          (Diagrams.rotate . (Diagrams.@@ Diagrams.deg) . realToFrac)
                          (ApertureDefinition.rotation params)
                          ( addHole
                              ( Diagrams.polygon
                                  ( Diagrams.PolygonOpts
                                      ( Diagrams.PolyRegular
                                          (ApertureDefinition.numberOfVertices params)
                                          (realToFrac $ ApertureDefinition.outerDiameter params / 2)
                                      )
                                      Diagrams.NoOrient
                                      (P (Linear.V2 0 0))
                                  )
                              )
                              (ApertureDefinition.polygonHoleDiameter params)
                          )
                )
    , Gerber.flashMacro =
        \polarity primitives ->
          withPolarity config polarity
            . Diagrams.lw Diagrams.none
            . renderMacro
            $ primitives
    , Gerber.fillRegion =
        \polarity start edges ->
          withPolarity
            config
            polarity
            ( Diagrams.lw Diagrams.none $
                Diagrams.strokeLocLoop
                  ( Diagrams.at
                      (Diagrams.closeLine (fromEdges start edges))
                      (realToFrac <$> Diagrams.p2 start)
                  )
            )
    , Gerber.translate =
        \x y ->
          Diagrams.translate (Diagrams.r2 (realToFrac x, realToFrac y))
    , mirror = \case
        Mirroring.MirrorNone -> id
        Mirroring.MirrorX -> Diagrams.scaleX (-1)
        Mirroring.MirrorY -> Diagrams.scaleY (-1)
        Mirroring.MirrorXY -> Diagrams.scaleY (-1) . Diagrams.scaleX (-1)
    , rotate = Diagrams.rotate . (Diagrams.@@ Diagrams.deg) . realToFrac
    , scale = Diagrams.scale . realToFrac
    }


fromEdges ::
  ( Diagrams.Transformable a
  , Monoid a
  , Diagrams.TrailLike a
  , RealFloat (Diagrams.N a)
  , Diagrams.V a ~ Linear.V2
  ) =>
  (Float, Float) ->
  [Edge.Edge] ->
  a
fromEdges _ [] =
  mempty
fromEdges currentPoint (Edge.Line end : edges) =
  (realToFrac <$> Diagrams.p2 currentPoint) Diagrams.~~ (realToFrac <$> Diagrams.p2 end)
    <> fromEdges end edges
fromEdges currentPoint (Edge.ArcCW center end : edges) =
  let
    toStart =
      Diagrams.p2 currentPoint .-. Diagrams.p2 center

    toEnd =
      Diagrams.p2 end .-. Diagrams.p2 center

    arc =
      if all Linear.nearZero (Diagrams.p2 currentPoint .-. Diagrams.p2 end)
        then -- S2.12 states:
        --
        -- Multi quadrant mode: mode defining how circular interpolation is
        -- performed. In this mode a circular arc is allowed to extend over more
        -- than 90°. If the start point of the arc is equal to the end point the arc
        -- is a full circle of 360°.

          Diagrams.arc
            (realToFrac <$> Diagrams.direction toStart)
            (1 Diagrams.@@ Diagrams.turn)
        else
          Diagrams.arcCW
            (realToFrac <$> Diagrams.direction toStart)
            (realToFrac <$> Diagrams.direction toEnd)
   in
    if Linear.nearZero toStart
      then fromEdges end edges
      else Diagrams.scale (realToFrac (Diagrams.norm toStart)) arc <> fromEdges end edges
fromEdges currentPoint (Edge.ArcCCW center end : edges) =
  let
    toStart =
      Diagrams.p2 currentPoint .-. Diagrams.p2 center

    toEnd =
      Diagrams.p2 end .-. Diagrams.p2 center

    arc =
      if all Linear.nearZero (Diagrams.p2 currentPoint .-. Diagrams.p2 end)
        then -- S2.12 states:
        --
        -- Multi quadrant mode: mode defining how circular interpolation is
        -- performed. In this mode a circular arc is allowed to extend over more
        -- than 90°. If the start point of the arc is equal to the end point the arc
        -- is a full circle of 360°.

          Diagrams.arc
            (realToFrac <$> Diagrams.direction toStart)
            (1 Diagrams.@@ Diagrams.turn)
        else
          Diagrams.arcCCW
            (realToFrac <$> Diagrams.direction toStart)
            (realToFrac <$> Diagrams.direction toEnd)
   in
    if Linear.nearZero toStart
      then fromEdges end edges
      else Diagrams.scale (realToFrac (Diagrams.norm toStart)) arc <> fromEdges end edges


data Config = Config
  { clearColour :: Diagrams.Colour Double
  , darkColour :: Diagrams.Colour Double
  }


defaultConfig :: Config
defaultConfig =
  Config
    { clearColour = Diagrams.white
    , darkColour = Diagrams.black
    }


withPolarity ::
  ( Diagrams.V c ~ Diagrams.V2
  , Floating (Diagrams.N c)
  , Typeable (Diagrams.N c)
  , Diagrams.HasStyle c
  ) =>
  Config ->
  Polarity.Polarity ->
  c ->
  c
withPolarity Config{clearColour} Polarity.Clear =
  Diagrams.lc Diagrams.white . Diagrams.fc clearColour
withPolarity Config{darkColour} Polarity.Dark =
  Diagrams.lc darkColour . Diagrams.fc darkColour


strokeAperture ::
  ( Diagrams.HasStyle c
  , Typeable (Diagrams.N c)
  , Fractional (Diagrams.N c)
  , Monoid c
  ) =>
  ApertureDefinition.ApertureDefinition ->
  c ->
  c
strokeAperture aperture =
  case aperture of
    ApertureDefinition.Circle params ->
      Diagrams.lwG
        (realToFrac (ApertureDefinition.diameter params))
        . Diagrams.lineCap Diagrams.LineCapRound
    ApertureDefinition.Rectangle params ->
      Diagrams.lwG
        (realToFrac (ApertureDefinition.width params))
        . Diagrams.lineCap Diagrams.LineCapSquare
    _ ->
      const mempty -- Only circles and rectangles can be stroked


renderMacro ::
  Diagrams.Renderable (Diagrams.Path V2 Double) b =>
  [MacroDefinition.Primitive MacroDefinition.Exposure Float] ->
  Diagrams.QDiagram b V2 Double Diagrams.Any
-- A whole macro is flashed with either dark or clear polarity, primitives with exposure off remove areas where flashing occur,
-- so all primitives are generated as paths and then combined with 'Diagrams.TwoD.Path.Boolean' to make a single diagram filled
-- everywhere with the same polarity.
renderMacro = foldMap Diagrams.stroke . foldl' addToOnPaths [] . concatMap (explode . toPath . fmap realToFrac)
  where
    explode (a, bs) = (a,) <$> bs

    toPath :: MacroDefinition.Primitive MacroDefinition.Exposure Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
    toPath = \case
      MacroDefinition.Circle a ->
        pathCircle a
      MacroDefinition.VectorLine a ->
        pathVectorLine a
      MacroDefinition.CenterLine a ->
        pathCenterLine a
      MacroDefinition.Outline a ->
        pathOutline a
      MacroDefinition.Polygon a ->
        pathPolygon a
      MacroDefinition.Moire a ->
        pathMoire a
      MacroDefinition.Thermal a ->
        pathThermal a
      where
        rotate = Diagrams.rotate . (Diagrams.@@ Diagrams.deg)

        maybeRotate = maybe id rotate

        pathCircle :: MacroDefinition.CircleModifiers MacroDefinition.Exposure Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
        pathCircle a =
          ( MacroDefinition.circleExposure a
          ,
            [ maybeRotate
                (MacroDefinition.circleRotation a)
                ( Diagrams.translate
                    (MacroDefinition.circleCenter a)
                    (safeCircle (MacroDefinition.diameter a / 2))
                )
            ]
          )

        pathVectorLine :: MacroDefinition.VectorLineModifiers MacroDefinition.Exposure Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
        pathVectorLine a =
          ( MacroDefinition.vectorLineExposure a
          ,
            [ rotate
                (MacroDefinition.vectorLineRotation a)
                ( Diagrams.translate
                    ((s ^+^ e) / 2)
                    ( Diagrams.rotate
                        theta
                        (Diagrams.rect (distance s e) (MacroDefinition.vectorLineWidth a))
                    )
                )
            ]
          )
          where
            s = MacroDefinition.start a
            e = MacroDefinition.end a
            V2 dx dy = e ^-^ s
            theta = Diagrams.atan2A dy dx

        pathCenterLine :: MacroDefinition.CenterLineModifiers MacroDefinition.Exposure Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
        pathCenterLine a =
          ( MacroDefinition.centerLineExposure a
          ,
            [ rotate
                (MacroDefinition.centerLineRotation a)
                ( Diagrams.translate
                    (MacroDefinition.centerLineCenter a)
                    (Diagrams.rect (MacroDefinition.centerLineWidth a) (MacroDefinition.height a))
                )
            ]
          )

        pathOutline :: MacroDefinition.OutlineModifiers MacroDefinition.Exposure Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
        pathOutline a =
          ( MacroDefinition.outlineExposure a
          ,
            [ rotate
                (MacroDefinition.outlineRotation a)
                (Diagrams.toPath $ Diagrams.closeTrail (Diagrams.fromVertices points) `Diagrams.at` firstPoint)
            ]
          )
          where
            firstPoint NonEmpty.:| restPoints = P <$> MacroDefinition.vertices a
            points = firstPoint : restPoints

        pathPolygon :: MacroDefinition.PolygonModifiers MacroDefinition.Exposure Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
        pathPolygon a =
          ( MacroDefinition.polygonExposure a
          ,
            [ rotate
                (MacroDefinition.polygonRotation a)
                ( Diagrams.polygon
                    ( Diagrams.PolygonOpts
                        ( Diagrams.PolyRegular
                            (MacroDefinition.numVertices a)
                            (MacroDefinition.polygonDiameter a / 2)
                        )
                        Diagrams.NoOrient
                        (P (Linear.V2 0 0))
                    )
                )
            ]
          )

        pathMoire :: MacroDefinition.MoireModifiers Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
        pathMoire a =
          (MacroDefinition.ExposureOn,) $
            rotate (MacroDefinition.moireRotation a) . Diagrams.translate (MacroDefinition.moireCenter a) <$> crossHair ++ rings
          where
            rings = take (round $ MacroDefinition.maximumNumberOfRings a) . unfoldr oneRing . (/ 2) . MacroDefinition.outerRingDiameter $ a

            oneRing outer
              | outer <= 0 = Nothing
              | otherwise = Just (ring, nextOuter)
              where
                inner = outer - MacroDefinition.ringThickness a
                ring = Diagrams.annularWedge outer inner Diagrams.xDir (1 Diagrams.@@ Diagrams.turn)
                nextOuter = inner - MacroDefinition.ringGap a

            crossHair
              | l <= 0 || t <= 0 = []
              | otherwise =
                  [ Diagrams.rect l t
                  , Diagrams.rect t l
                  ]
              where
                t = MacroDefinition.crossThickness a
                l = MacroDefinition.crossLength a

        pathThermal :: MacroDefinition.ThermalModifiers Double -> (MacroDefinition.Exposure, [Diagrams.Path V2 Double])
        pathThermal a =
          ( MacroDefinition.ExposureOn
          ,
            [ rotate
                (MacroDefinition.thermalRotation a)
                ( Diagrams.translate (MacroDefinition.thermalCenter a) thermal
                )
            ]
          )
          where
            thermal = foldl' (Diagrams.Boolean.difference Diagrams.EvenOdd) outerCircle [hgap, vgap, innerCircle]

            outerDim = MacroDefinition.outerDiameter a

            outerCircle = safeCircle (outerDim / 2)

            innerCircle = safeCircle (MacroDefinition.innerDiameter a / 2)

            thickness = MacroDefinition.gapThickness a

            length_ = outerDim * 1.1

            hgap = Diagrams.rect length_ thickness

            vgap = Diagrams.rect thickness length_


-- Add a new path to the list of paths.
--
-- If it has exposure on its merely added to the list.
--
-- If it has exposure off its subtracted from any paths currently in the list
-- which it intersects.
addToOnPaths ::
  [Diagrams.Path V2 Double] ->
  (MacroDefinition.Exposure, Diagrams.Path V2 Double) ->
  [Diagrams.Path V2 Double]
addToOnPaths ps (MacroDefinition.ExposureOn, p) = ps ++ [p]
addToOnPaths [] (MacroDefinition.ExposureOff, _) = []
addToOnPaths ps (MacroDefinition.ExposureOff, pOff) = map subtractOff ps
  where
    subtractOff pOn
      | intersecting = subtractOff'
      | onInsideOff = subtractOff'
      | otherwise = pOn
      where
        subtractOff' = Diagrams.Boolean.difference Diagrams.EvenOdd pOn pOff

        intersecting = not . null $ Diagrams.intersectPoints pOn pOff

        onInsideOff = any (Diagrams.contains' onBoundinbBox) offPoints
          where
            onBoundinbBox = Diagrams.boundingBox pOn
            offPoints = concat (Diagrams.pathPoints pOff)
