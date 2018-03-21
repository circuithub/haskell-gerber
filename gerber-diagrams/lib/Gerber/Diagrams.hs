{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NoMonomorphismRestriction #-}

module Gerber.Diagrams where

import Data.Monoid ( (<>) )
import Data.Typeable ( Typeable )
import Linear ( (^-^), (^+^) )
import Linear.Affine ( (.-.) )

import qualified Diagrams.Prelude as Diagrams
import qualified Linear

import qualified Gerber.ApertureDefinition as ApertureDefinition
import qualified Gerber.Evaluate as Gerber
import qualified Gerber.Evaluate.Edge as Edge
import qualified Gerber.Polarity as Polarity

import Debug.Trace


gerberToDiagram
  :: Diagrams.Renderable ( Diagrams.Path Diagrams.V2 Double ) b
  => Gerber.Evaluator ( Diagrams.QDiagram b Diagrams.V2 Double Diagrams.Any )
gerberToDiagram =
  Gerber.Evaluator
    { Gerber.line =
        \polarity aperture (x1, y1) (x2, y2) -> 
          withPolarity
            polarity 
            ( case aperture of
                ApertureDefinition.Circle params -> 
                  Diagrams.lwG ( realToFrac ( ApertureDefinition.diameter params ) ) $
                  Diagrams.lineCap Diagrams.LineCapRound $
                  Diagrams.strokeLocTrail
                    ( Diagrams.P ( realToFrac <$> (Diagrams.V2 x1 y1) )
                        Diagrams.~~ Diagrams.P (realToFrac <$> Diagrams.V2 x2 y2)
                    )

                ApertureDefinition.Rectangle params -> 
                  Diagrams.lwG ( realToFrac ( ApertureDefinition.width params ) ) $
                  Diagrams.lineCap Diagrams.LineCapSquare $
                  Diagrams.strokeLocTrail
                    ( Diagrams.P ( realToFrac <$> (Diagrams.V2 x1 y1) )
                        Diagrams.~~ Diagrams.P (realToFrac <$> Diagrams.V2 x2 y2)
                    )

                _ ->
                  mempty -- Only circles and rectangles can be stroked
            )
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
    }


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

  in
  Diagrams.scale
    ( realToFrac ( Diagrams.norm toStart ) )
    ( Diagrams.arcCCW
        ( realToFrac <$> Diagrams.direction toStart )
        ( realToFrac <$> Diagrams.direction toEnd )
    )
    <> fromEdges end edges
  


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
