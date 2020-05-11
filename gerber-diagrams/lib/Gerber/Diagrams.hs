{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NoMonomorphismRestriction #-}

module Gerber.Diagrams where

import Data.Monoid ( (<>) )
import Data.Typeable ( Typeable )
import Linear.Affine ( (.-.) )

import qualified Diagrams.Prelude as Diagrams

import qualified Gerber.ApertureDefinition as ApertureDefinition
import qualified Gerber.Evaluate as Gerber
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.Polarity as Polarity
import qualified Linear


gerberToDiagram
  :: Diagrams.Renderable ( Diagrams.Path Diagrams.V2 Double ) b
  => Gerber.Evaluator ( Diagrams.QDiagram b Diagrams.V2 Double Diagrams.Any )
gerberToDiagram =
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
              if not ( Linear.nearZero ( Diagrams.norm toStart ) ) then
                Diagrams.translate
                  ( realToFrac <$> Diagrams.r2 center )
                  ( Diagrams.scale
                      ( realToFrac ( Diagrams.norm toStart ) )
                      ( if Linear.nearZero ( toStart .-. toEnd ) then
                          Diagrams.arc'
                            1
                            ( realToFrac <$> Diagrams.direction toStart )
                            ( 1 Diagrams.@@ Diagrams.turn )

                        else
                          Diagrams.arcCCW
                            ( realToFrac <$> Diagrams.direction toStart )
                            ( realToFrac <$> Diagrams.direction toEnd )
                      )
                  )

              else
                mempty

          in
          withPolarity polarity ( strokeAperture aperture arc )
    , Gerber.line =
        \polarity aperture (x1, y1) (x2, y2) ->
          let
            trail =
              Diagrams.strokeLocTrail
                ( Diagrams.P ( realToFrac <$> Diagrams.V2 x1 y1 )
                    Diagrams.~~ Diagrams.P (realToFrac <$> Diagrams.V2 x2 y2)
                )

          in
          withPolarity polarity ( strokeAperture aperture trail )
    , Gerber.flash =
        \polarity aperture (x, y) ->
          withPolarity
            polarity
            ( Diagrams.lw Diagrams.none $
              Diagrams.translate ( realToFrac <$> Diagrams.V2 x y ) $
              case aperture of
                ApertureDefinition.Circle params ->
                  Diagrams.circle ( realToFrac $ ApertureDefinition.diameter params / 2 )

                ApertureDefinition.Rectangle params ->
                  Diagrams.rect
                    ( realToFrac $ ApertureDefinition.width params )
                    ( realToFrac $ ApertureDefinition.height params )

                ApertureDefinition.Obround params ->
                  Diagrams.roundedRect
                    ( realToFrac $ ApertureDefinition.width params )
                    ( realToFrac $ ApertureDefinition.height params )
                    ( realToFrac
                        ( max
                            ( ApertureDefinition.width params )
                            ( ApertureDefinition.height params )
                        )
                    )

                -- TODO
                _ ->
                  mempty
            )
    , Gerber.fillRegion =
        \polarity start edges ->
          withPolarity
            polarity
            ( Diagrams.lw Diagrams.none $ Diagrams.strokeLocLoop
                ( Diagrams.at
                    ( Diagrams.closeLine ( fromEdges start edges ) )
                    ( realToFrac <$> Diagrams.p2 start )
                )
            )
    , Gerber.translate =
        \x y ->
          Diagrams.translate ( Diagrams.r2 ( realToFrac x, realToFrac y ) )
    }


fromEdges
  :: ( Diagrams.Transformable a
     , Monoid a
     , Diagrams.TrailLike a
     , RealFloat ( Diagrams.N a )
     , Diagrams.V a ~ Linear.V2
     )
  => ( Float, Float ) -> [ Edge.Edge ] -> a
fromEdges _ [] =
  mempty

fromEdges currentPoint ( Edge.Line end : edges ) =
  ( realToFrac <$> Diagrams.p2 currentPoint ) Diagrams.~~ ( realToFrac <$> Diagrams.p2 end )
    <> fromEdges end edges

fromEdges currentPoint ( Edge.ArcCW center end : edges ) =
  let
    toStart =
      Diagrams.p2 currentPoint .-. Diagrams.p2 center

    toEnd =
      Diagrams.p2 end .-. Diagrams.p2 center

  in
  Diagrams.scale
    ( realToFrac ( Diagrams.norm toStart ) )
    ( Diagrams.arcCW
        ( realToFrac <$> Diagrams.direction toStart )
        ( realToFrac <$> Diagrams.direction toEnd )
    )
    <> fromEdges end edges

fromEdges currentPoint ( Edge.ArcCCW center end : edges ) =
  let
    toStart =
      Diagrams.p2 currentPoint .-. Diagrams.p2 center

    toEnd =
      Diagrams.p2 end .-. Diagrams.p2 center

    arc =
      if Linear.nearZero ( Diagrams.p2 currentPoint .-. Diagrams.p2 end ) then
        -- S2.12 states:
        --
        -- Multi quadrant mode: mode defining how circular interpolation is
        -- performed. In this mode a circular arc is allowed to extend over more
        -- than 90°. If the start point of the arc is equal to the end point the arc
        -- is a full circle of 360°.
        Diagrams.arc
          ( realToFrac <$> Diagrams.direction toStart )
          ( 1 Diagrams.@@ Diagrams.turn )

      else
        Diagrams.arcCCW
          ( realToFrac <$> Diagrams.direction toStart )
          ( realToFrac <$> Diagrams.direction toEnd )

  in
  Diagrams.scale ( realToFrac ( Diagrams.norm toStart ) ) arc <> fromEdges end edges



withPolarity
  :: ( Diagrams.V c ~ Diagrams.V2
     , Floating (Diagrams.N c)
     , Typeable (Diagrams.N c)
     , Diagrams.HasStyle c
     )
  => Polarity.Polarity -> c -> c
withPolarity Polarity.Clear =
  Diagrams.lc Diagrams.white . Diagrams.fc Diagrams.white

withPolarity Polarity.Dark =
  Diagrams.lc Diagrams.black . Diagrams.fc Diagrams.black


strokeAperture
  :: ( Diagrams.HasStyle c
     , Typeable ( Diagrams.N c )
     , Fractional ( Diagrams.N c )
     , Monoid c
     )
  => ApertureDefinition.ApertureDefinition -> c -> c
strokeAperture aperture =
  case aperture of
    ApertureDefinition.Circle params ->
      Diagrams.lwG
        ( realToFrac ( ApertureDefinition.diameter params ) )
        . Diagrams.lineCap Diagrams.LineCapRound

    ApertureDefinition.Rectangle params ->
      Diagrams.lwG
        ( realToFrac ( ApertureDefinition.width params ) )
        . Diagrams.lineCap Diagrams.LineCapSquare

    _ ->
      const mempty -- Only circles and rectangles can be stroked
