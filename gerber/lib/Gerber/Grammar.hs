{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gerber.Grammar (
  parseGerber,
  parseGerberPretty,
) where

-- base
import Control.Applicative (empty, many, optional, some, (<|>))
import Control.Monad (guard, void)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (Void)
import Text.Read (readMaybe)

-- gerber
import qualified Gerber.ApertureDefinition as Gerber
import qualified Gerber.Attribute
import qualified Gerber.Attribute.AperFunction as AperFunction
import qualified Gerber.Attribute.CreationDate as Gerber
import qualified Gerber.Attribute.FileFunction as FileFunction
import qualified Gerber.Attribute.FileFunction.Copper as Gerber
import qualified Gerber.Attribute.FileFunction.Drill as Gerber.Drill
import qualified Gerber.Attribute.FileFunction.Index as FileFunction
import qualified Gerber.Attribute.FileFunction.Mask as Gerber.Mask
import qualified Gerber.Attribute.FileFunction.Profile as Gerber
import qualified Gerber.Attribute.FileFunction.Side as Gerber.Side
import qualified Gerber.Attribute.FilePolarity as FilePolarity
import qualified Gerber.Attribute.GenerationSoftware as Gerber.GenerationSoftware
import qualified Gerber.Attribute.MD5 as Gerber.MD5
import qualified Gerber.Attribute.Part as Gerber
import qualified Gerber.Attribute.ProjectId as Gerber.ProjectId
import qualified Gerber.Command as Gerber
import qualified Gerber.DCodeNumber as Gerber
import qualified Gerber.EncodedDecimal as Gerber
import qualified Gerber.Format as Gerber
import Gerber.MacroDefinition as MacroDefinition
import qualified Gerber.Mirroring as Gerber
import qualified Gerber.Movement as Gerber
import qualified Gerber.Padding as Padding
import qualified Gerber.Polarity as Gerber
import qualified Gerber.StepRepeat as Gerber
import qualified Gerber.Unit

-- linear
import Linear.V2 (V2 (..))

-- megaparsec
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Megaparsec

-- text
import qualified Data.Text as StrictText


float :: Megaparsec.MonadParsec e StrictText.Text m => m Float
float = do
  negated <- maybe id (const negate) <$> optional (Megaparsec.char '-')

  intPart <-
    Megaparsec.takeWhile1P Nothing isDigit

  decPart <- optional $ do
    void (Megaparsec.string ".")

    Megaparsec.takeWhile1P Nothing isDigit

  case readMaybe
    (StrictText.unpack (intPart <> maybe "" ("." <>) decPart)) of
    Nothing ->
      empty
    Just a ->
      pure $ negated a


negative :: Num a => Megaparsec.MonadParsec e StrictText.Text m => m a -> m a
negative p =
  asum [negate <$ Megaparsec.char '-', pure id]
    <*> p


int :: Megaparsec.MonadParsec e StrictText.Text m => m Int
int =
  read . StrictText.unpack <$> Megaparsec.takeWhile1P Nothing isDigit


index :: Megaparsec.MonadParsec e StrictText.Text m => m FileFunction.Index
index =
  Megaparsec.label "index" $
    FileFunction.Index <$> read . StrictText.unpack <$> Megaparsec.takeWhile1P Nothing isDigit


string :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
string = Megaparsec.label "string" $ StrictText.pack <$> many (Megaparsec.noneOf ['*', '%'])


newlines :: Megaparsec.MonadParsec e StrictText.Text m => m ()
newlines =
  void (Megaparsec.takeWhileP Nothing (`elem` ("\n\r" :: String)))


endOfBlock :: Megaparsec.MonadParsec e StrictText.Text m => m ()
endOfBlock =
  Megaparsec.char '*' *> newlines


format :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Format
format = Megaparsec.label "format" $ do
  integerPositions <-
    read . pure <$> Megaparsec.oneOf ['1' .. '6'] <?> "[1-6]"

  decimalPositions <-
    read . pure <$> Megaparsec.oneOf ['1' .. '6'] <?> "[5-6]"

  return Gerber.Format{..}


fs :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
fs = Megaparsec.label "FS" $ do
  _ <- Megaparsec.string "%FS"
  padding <-
    asum
      [ Padding.PadLeading <$ Megaparsec.string "LA"
      , Padding.PadTrailing <$ Megaparsec.string "TA"
      ]
  _ <- Megaparsec.char 'X'
  x <- format
  _ <- Megaparsec.char 'Y'
  y <- format
  _ <- Megaparsec.string "*%"
  newlines
  pure $ Gerber.FS padding x y


mo :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
mo =
  Megaparsec.label "MO" $
    Gerber.MO
      <$ Megaparsec.string "%MO"
      <*> ( Gerber.Unit.MM <$ Megaparsec.string "MM"
              <|> Gerber.Unit.IN <$ Megaparsec.string "IN"
          )
      <* Megaparsec.string "*%"
      <* newlines


ad :: forall e m. Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ad = Megaparsec.label "AD" $ do
  let
    dCodeNumber =
      Gerber.DCodeNumber <$> int

    circle =
      Gerber.Circle
        <$ Megaparsec.char 'C'
        <* Megaparsec.char ','
        <*> ( Gerber.CircleModifiers
                <$> float
                <*> optional (Megaparsec.char 'X' *> float)
            )

    polygonModifiers =
      Gerber.PolygonModifiers
        <$> float
        <* Megaparsec.char 'X'
        <*> int
        <*> optional (Megaparsec.char 'X' *> float)
        <*> optional (Megaparsec.char 'X' *> float)

    rectangleModifiers =
      Gerber.RectangleModifiers
        <$> float
        <* Megaparsec.char 'X'
        <*> float
        <*> optional (Megaparsec.char 'X' *> float)

    rectangle :: m Gerber.ApertureDefinition
    rectangle =
      Gerber.Rectangle
        <$ Megaparsec.char 'R'
        <* Megaparsec.char ','
        <*> rectangleModifiers

    obround :: m Gerber.ApertureDefinition
    obround =
      Gerber.Obround
        <$ Megaparsec.char 'O'
        <* Megaparsec.char ','
        <*> rectangleModifiers

    polygon :: m Gerber.ApertureDefinition
    polygon =
      Gerber.Polygon
        <$ Megaparsec.char 'P'
        <* Megaparsec.char ','
        <*> polygonModifiers

    macro :: Gerber.DCodeNumber -> m Gerber.Command
    macro n =
      Megaparsec.try $
        Gerber.MacroAD n
          <$> Megaparsec.takeWhile1P Nothing (`notElem` [',', '*'])
          <* optional (Megaparsec.char ',')
          <*> Megaparsec.sepBy float (Megaparsec.char 'X')

  n <- Megaparsec.string "%AD" *> Megaparsec.char 'D' *> dCodeNumber
  let ad' = Megaparsec.try . fmap (Gerber.AD n)
  asum
    [ ad' circle
    , ad' rectangle
    , ad' obround
    , ad' polygon
    , macro n
    ]
    <* Megaparsec.string "*%"
    <* newlines


g01 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g01 =
  Megaparsec.label "G01" $
    Gerber.G01
      <$ Megaparsec.string "G01"
      <* endOfBlock


g03 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g03 =
  Megaparsec.label "G03" $
    Gerber.G03
      <$ Megaparsec.string "G03"
      <* endOfBlock


g36 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g36 =
  Megaparsec.label "G36" $
    Gerber.G36
      <$ Megaparsec.string "G36"
      <* endOfBlock


g37 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g37 =
  Megaparsec.label "G37" $
    Gerber.G37
      <$ Megaparsec.string "G37"
      <* endOfBlock


g71 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g71 =
  Megaparsec.label "G71" $
    Gerber.G71
      <$ Megaparsec.string "G71"
      <* endOfBlock


g74 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g74 =
  Megaparsec.label "G74" $
    Gerber.G74
      <$ Megaparsec.string "G74"
      <* endOfBlock


g75 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g75 =
  Megaparsec.label "G75" $
    Gerber.G75
      <$ Megaparsec.string "G75"
      <* endOfBlock


m02 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
m02 =
  Megaparsec.label "M02" $
    Gerber.M02
      <$ Megaparsec.string "M02"
      <* endOfBlock


g04 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g04 =
  Megaparsec.label "G04" $
    Gerber.G04
      <$ (Megaparsec.string "G04" <|> Megaparsec.string "LN" <|> Megaparsec.string "IN")
      <*> string
      <* Megaparsec.char '*'
      <* newlines


lp :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
lp =
  Megaparsec.label "LP" $
    Gerber.LP
      <$ Megaparsec.string "%LP"
      <*> polarity
      <* Megaparsec.string "*%"
      <* newlines


polarity :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Polarity
polarity =
  Megaparsec.label "polarity" $
    asum
      [ Gerber.Clear <$ Megaparsec.char 'C'
      , Gerber.Dark <$ Megaparsec.char 'D'
      ]


d :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d =
  Gerber.D
    <$ optional (Megaparsec.string "G54")
    <* Megaparsec.char 'D'
    <*> ( do
            n <-
              read . StrictText.unpack
                <$> ( StrictText.cons
                        <$> Megaparsec.satisfy isDigit
                        <*> Megaparsec.takeWhile1P Nothing isDigit
                    )

            guard (n >= 10)

            return (Gerber.DCodeNumber n)
        )
    <* endOfBlock


movement :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Movement
movement =
  Gerber.Movement
    <$> optional (Megaparsec.char 'X' *> encodedDecimal)
    <*> optional (Megaparsec.char 'Y' *> encodedDecimal)
    <*> optional (Megaparsec.char 'I' *> encodedDecimal)
    <*> optional (Megaparsec.char 'J' *> encodedDecimal)
  where
    encodedDecimal = do
      negative_ <-
        asum
          [ True <$ Megaparsec.char '-'
          , pure False
          ]

      digits <-
        map digitToInt . StrictText.unpack <$> Megaparsec.takeWhileP Nothing isDigit

      return Gerber.EncodedDecimal{negative = negative_, ..}


stepRepeat :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.StepRepeat
stepRepeat =
  Gerber.StepRepeat
    <$> (Megaparsec.char 'X' *> int)
    <*> (Megaparsec.char 'Y' *> int)
    <*> (Megaparsec.char 'I' *> negative float)
    <*> (Megaparsec.char 'J' *> negative float)


d01 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d01 =
  Megaparsec.label "D01" $
    Gerber.D01
      <$> movement
      <* optional (Megaparsec.string "D01" <|> Megaparsec.string "D1")
      <* endOfBlock


d02 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d02 =
  Megaparsec.label "D02" $
    Gerber.D02
      <$> movement
      <* (Megaparsec.string "D02" <|> Megaparsec.string "D2")
      <* endOfBlock


d03 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d03 =
  Megaparsec.label "D03" $
    Gerber.D03
      <$> movement
      <* (Megaparsec.string "D03" <|> Megaparsec.string "D3")
      <* endOfBlock


am :: forall e m. Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
am =
  Megaparsec.label "AM" $
    Gerber.AM
      <$ Megaparsec.string "%AM"
      <*> (StrictText.pack <$> Megaparsec.someTill Megaparsec.anySingle endOfBlock')
      <*> Megaparsec.sepEndBy content endOfBlock'
      <* Megaparsec.string "%"
      <* newlines
  where
    endOfBlock' :: m ()
    endOfBlock' = endOfBlock >> newlines

    content :: m (MacroDefinition.Definition MacroDefinition.Modifier MacroDefinition.Modifier)
    content =
      (asum . map Megaparsec.try)
        [ MacroDefinition.Variable <$> (Megaparsec.char '$' >> Megaparsec.decimal <* Megaparsec.char '=') <*> modifier
        , MacroDefinition.Comment <$ checkCode 0 <*> Megaparsec.takeWhileP Nothing (/= '*')
        , MacroDefinition.Primitive . MacroDefinition.Circle <$> circle
        , MacroDefinition.Primitive . MacroDefinition.VectorLine <$> vectorLine
        , MacroDefinition.Primitive . MacroDefinition.CenterLine <$> centerLine
        , MacroDefinition.Primitive . MacroDefinition.Outline <$> outline
        , MacroDefinition.Primitive . MacroDefinition.Polygon <$> polygon
        , MacroDefinition.Primitive . MacroDefinition.Moire <$> moire
        , MacroDefinition.Primitive . MacroDefinition.Thermal <$> thermal
        , invalidDefinition
        ]
      where
        checkCode :: Int -> m ()
        checkCode code = Megaparsec.decimal >>= \a -> guard (a == code)

        comma :: m ()
        comma = void (Megaparsec.char ',' >> newlines)

        coordinatePair :: m (V2 MacroDefinition.Modifier)
        coordinatePair = V2 <$> (modifier <* comma) <*> modifier

        circle :: m (MacroDefinition.CircleModifiers MacroDefinition.Modifier MacroDefinition.Modifier)
        circle =
          MacroDefinition.CircleModifiers
            <$ (checkCode 1 <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> coordinatePair
            <*> optional (comma >> modifier)

        vectorLine :: m (MacroDefinition.VectorLineModifiers MacroDefinition.Modifier MacroDefinition.Modifier)
        vectorLine =
          MacroDefinition.VectorLineModifiers
            <$ (checkCode 20 <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (coordinatePair <* comma)
            <*> (coordinatePair <* comma)
            <*> modifier

        centerLine :: m (MacroDefinition.CenterLineModifiers MacroDefinition.Modifier MacroDefinition.Modifier)
        centerLine =
          MacroDefinition.CenterLineModifiers
            <$ (checkCode 21 <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (coordinatePair <* comma)
            <*> modifier

        outline :: m (MacroDefinition.OutlineModifiers MacroDefinition.Modifier MacroDefinition.Modifier)
        outline =
          MacroDefinition.OutlineModifiers
            <$ (checkCode 4 <* comma)
            <*> (modifier <* comma)
            <*> (vertices <* comma)
            <*> modifier
          where
            vertices = do
              -- from the spec The number of vertices of the outline = the number of coordinate pairs minus one.  An integer ≥3.
              numVertices <- Megaparsec.decimal <* comma
              sequence . (coordinatePair :|) . replicate numVertices $ comma >> coordinatePair

        polygon :: m (MacroDefinition.PolygonModifiers MacroDefinition.Modifier MacroDefinition.Modifier)
        polygon =
          MacroDefinition.PolygonModifiers
            <$ (checkCode 5 <* comma)
            <*> (modifier <* comma)
            <*> (Megaparsec.decimal <* comma)
            <*> (coordinatePair <* comma)
            <*> (modifier <* comma)
            <*> modifier

        moire :: m (MacroDefinition.MoireModifiers MacroDefinition.Modifier)
        moire =
          MacroDefinition.MoireModifiers
            <$ (checkCode 6 <* comma)
            <*> (coordinatePair <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> modifier

        thermal :: m (MacroDefinition.ThermalModifiers MacroDefinition.Modifier)
        thermal =
          MacroDefinition.ThermalModifiers
            <$ (checkCode 7 <* comma)
            <*> (coordinatePair <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> (modifier <* comma)
            <*> modifier

        invalidDefinition :: m (MacroDefinition.Definition MacroDefinition.Modifier MacroDefinition.Modifier)
        invalidDefinition =
          MacroDefinition.InvalidDefinition
            <$> ((Megaparsec.decimal >>= \a -> guard (a `notElem` [0, 1, 4, 5, 7, 6, 20, 21]) >> pure a) <* comma)
            <*> Megaparsec.takeWhileP Nothing (/= '*')

        modifier :: m MacroDefinition.Modifier
        modifier = fixAssociation <$> unaryOrBinaryModifier
          where
            unaryOrBinaryModifier :: m MacroDefinition.Modifier
            unaryOrBinaryModifier = do
              a <-
                asum $
                  map
                    Megaparsec.try
                    [ unaryModifier
                    , UnaryPlus <$ Megaparsec.char '+' <*> unaryModifier
                    , UnaryMinus <$ Megaparsec.char '-' <*> unaryModifier
                    ]
              binayrModifier a <|> pure a

            unaryModifier :: m MacroDefinition.Modifier
            unaryModifier =
              asum $
                map
                  Megaparsec.try
                  [ Megaparsec.between (Megaparsec.char '(') (Megaparsec.char ')') (Parentheses <$> unaryOrBinaryModifier)
                  , Decimal <$> float
                  , VariableReference <$ Megaparsec.char '$' <*> Megaparsec.decimal
                  ]

            binayrModifier :: Modifier -> m MacroDefinition.Modifier
            binayrModifier m =
              asum $
                map
                  Megaparsec.try
                  [ Plus m <$ Megaparsec.char '+' <*> unaryOrBinaryModifier
                  , Minus m <$ Megaparsec.char '-' <*> unaryOrBinaryModifier
                  , Multiply m <$ Megaparsec.satisfy (`elem` ['x', 'X']) <*> unaryOrBinaryModifier
                  , Divide m <$ Megaparsec.char '/' <*> unaryOrBinaryModifier
                  ]

            -- we parse left biased so after parsing we rebalance according to associativity
            -- so that the expression tree is in the correct evaluation order
            fixAssociation :: MacroDefinition.Modifier -> MacroDefinition.Modifier
            fixAssociation stable
              | reducedOnce == stable = stable
              | otherwise = fixAssociation reducedOnce
              where
                reducedOnce = go stable

                go = \case
                  MacroDefinition.Decimal x ->
                    MacroDefinition.Decimal x
                  VariableReference x ->
                    VariableReference x
                  Parentheses x ->
                    Parentheses (go x)
                  UnaryPlus x ->
                    UnaryPlus (go x)
                  UnaryMinus x ->
                    UnaryMinus (go x)
                  Plus l r ->
                    Plus (go l) (go r)
                  Minus l r ->
                    case go r of
                      Plus rl rr ->
                        Plus (Minus (go l) rl) rr
                      _ ->
                        Minus (go l) (go r)
                  Multiply l r ->
                    case go r of
                      Plus rl rr ->
                        Plus (Multiply (go l) rl) rr
                      Minus rl rr ->
                        Minus (Multiply (go l) rl) rr
                      _ ->
                        Multiply (go l) (go r)
                  Divide l r ->
                    case go r of
                      Plus rl rr ->
                        Plus (Divide (go l) rl) rr
                      Minus rl rr ->
                        Minus (Divide (go l) rl) rr
                      Multiply rl rr ->
                        Multiply (Divide (go l) rl) rr
                      _ ->
                        Divide (go l) (go r)


sr :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
sr =
  Megaparsec.label "SR" $
    Gerber.SR
      <$ Megaparsec.string "%SR"
      <*> stepRepeat
      <* Megaparsec.string "*%"
      <* newlines


srEnd :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
srEnd =
  Megaparsec.label "SR end" $
    Gerber.SR_End
      <$ Megaparsec.string "%SR*%"
      <* newlines


sf :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
sf =
  Megaparsec.label "SF" $
    Gerber.SF
      <$ Megaparsec.string "SF"
      <* optional (Megaparsec.string "A" *> float >>= guard . (== 1))
      <* optional (Megaparsec.string "B" *> float >>= guard . (== 1))
      <* endOfBlock


mi :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
mi =
  Megaparsec.label "MI" $
    Gerber.MI
      <$ Megaparsec.string "MI"
      <* optional (Megaparsec.string "A" *> Megaparsec.string "0")
      <* optional (Megaparsec.string "B" *> Megaparsec.string "0")
      <* endOfBlock


of_ :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
of_ =
  Megaparsec.label "OF" $
    Gerber.OF
      <$ Megaparsec.string "%OF"
      <*> optional (Megaparsec.string "A" *> float)
      <*> optional (Megaparsec.string "B" *> float)
      <* Megaparsec.string "*%"
      <* newlines


in_ :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
in_ =
  Megaparsec.label "IN" $
    Gerber.Deprecated . Gerber.IN
      <$ Megaparsec.string "%IN"
      <*> string
      <* Megaparsec.string "*%"
      <* newlines


ip :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ip =
  Megaparsec.label "IP" $
    Gerber.Deprecated Gerber.IP
      <$ Megaparsec.string "%IP"
      <* (Megaparsec.string "POS" <|> Megaparsec.string "NEG")
      <* Megaparsec.string "*%"
      <* newlines


ab :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ab =
  Megaparsec.label "AB" $
    Gerber.AB
      <$ Megaparsec.string "%ABD"
      <*> (Gerber.DCodeNumber <$> int)
      <* Megaparsec.string "*%"
      <* newlines


abEnd :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
abEnd =
  Megaparsec.label "AB end" $
    Gerber.AB_End
      <$ Megaparsec.string "%AB*%"
      <* newlines


lm :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
lm =
  Megaparsec.label "LM" $
    Gerber.LM
      <$ Megaparsec.string "%LM"
      <*> (asum . map Megaparsec.try)
        [ Gerber.MirrorXY <$ Megaparsec.string "XY"
        , Gerber.MirrorNone <$ Megaparsec.char 'N'
        , Gerber.MirrorX <$ Megaparsec.char 'X'
        , Gerber.MirrorY <$ Megaparsec.char 'Y'
        ]
      <* Megaparsec.string "*%"
      <* newlines


lr :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
lr =
  Megaparsec.label "LR" $
    Gerber.LR
      <$ Megaparsec.string "%LR"
      <*> float
      <* Megaparsec.string "*%"
      <* newlines


ls :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ls =
  Megaparsec.label "LS" $
    Gerber.LS
      <$ Megaparsec.string "%LS"
      <*> float
      <* Megaparsec.string "*%"
      <* newlines


command :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
command =
  asum
    ( map
        Megaparsec.try
        [ g01
        , g04
        , d
        , d01
        , d02
        , d03
        , g36
        , g37
        , g74
        , g75
        , g03
        , g71
        , sf
        , mi
        ]
    )


to :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
to = Megaparsec.label "TO" $ do
  _ <- Megaparsec.string "%TO"
  name <- objectAttributeName
  fields <- Megaparsec.many field
  _ <- Megaparsec.string "*%"

  newlines

  pure $ Gerber.TO name fields


tf :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
tf = Megaparsec.label "TF" $ do
  _ <- Megaparsec.string "%TF."
  attr <- fileAttribute
  _ <- Megaparsec.string "*%"
  newlines

  pure $ Gerber.TF attr


field :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
field = StrictText.pack <$> (Megaparsec.string "," *> many (Megaparsec.noneOf [',', '*', '%']))


username :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
username =
  let
    lowerUpper =
      Megaparsec.oneOf ['a' .. 'z']
        <|> Megaparsec.oneOf ['A' .. 'Z']

    startChar =
      Megaparsec.char '_' <|> lowerUpper <|> Megaparsec.char '$'

    otherChar =
      Megaparsec.char '.' <|> Megaparsec.char '_' <|> lowerUpper <|> Megaparsec.oneOf ['0' .. '9']
   in
    Megaparsec.label "username" $ do
      first <- startChar
      rest <- many otherChar
      pure $ StrictText.pack (first : rest)


fileAttribute :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Attribute.FileAttribute
fileAttribute =
  asum $
    map
      Megaparsec.try
      [ part
      , fileFunction
      , Gerber.Attribute.FilePolarity <$> filePolarity
      , Gerber.Attribute.SameCoordinates <$> sameCoordinates
      , Gerber.Attribute.CreationDate <$> Gerber.creationDateParser
      , generationSoftware
      , Gerber.Attribute.ProjectId <$> Gerber.ProjectId.projectIdParser
      , Gerber.Attribute.MD5 <$> Gerber.MD5.parser
      ]
  where
    part = do
      _ <- Megaparsec.string "Part,"
      asum $
        map
          Megaparsec.try
          [ Gerber.Attribute.Part Gerber.Single <$ Megaparsec.string "Single"
          , Gerber.Attribute.Part Gerber.Array <$ Megaparsec.string "Array"
          , Gerber.Attribute.Part Gerber.FabricationPanel <$ Megaparsec.string "FabricationPanel"
          , Gerber.Attribute.Part Gerber.Coupon <$ Megaparsec.string "Coupon"
          , Gerber.Attribute.Part . Gerber.Other <$> (Megaparsec.string "Other" *> field)
          ]

    fileFunction = do
      _ <- Megaparsec.string "FileFunction,"
      asum $
        map
          (fmap Gerber.Attribute.FileFunction . Megaparsec.try)
          [ fmap FileFunction.Copper $
              Gerber.Copper
                <$> (Megaparsec.string "Copper,L" *> index <* Megaparsec.char ',')
                <*> markParser
                <*> optional (Megaparsec.char ',' *> typeParser)
          , fmap FileFunction.Plated $
              Gerber.Drill.Drill
                <$> (Megaparsec.string "Plated," *> index)
                <*> (Megaparsec.char ',' *> index)
                <*> (Megaparsec.char ',' *> viaParser)
                <*> optional (Megaparsec.char ',' *> drillTypeParser)
          , fmap FileFunction.NonPlated $
              Gerber.Drill.Drill
                <$> (Megaparsec.string "NonPlated," *> index)
                <*> (Megaparsec.char ',' *> index)
                <*> (Megaparsec.char ',' *> viaParser)
                <*> optional (Megaparsec.char ',' *> drillTypeParser)
          , fmap FileFunction.Profile $
              Megaparsec.string "Profile," *> profileParser
          , fmap FileFunction.Soldermask $
              Megaparsec.string "Soldermask," *> maskParser
          , fmap FileFunction.Legend $
              Megaparsec.string "Legend," *> maskParser
          , FileFunction.Component
              <$> (Megaparsec.string "Component,L" *> index)
              <*> sideParser
          , FileFunction.Paste
              <$> (Megaparsec.string "Paste," *> sideParser)
          , FileFunction.Glue
              <$> (Megaparsec.string "Glue," *> sideParser)
          , FileFunction.Carbonmask
              <$> (Megaparsec.string "Carbonmask," *> maskParser)
          , FileFunction.Goldmask
              <$> (Megaparsec.string "Goldmask," *> maskParser)
          , FileFunction.Heatsink
              <$> (Megaparsec.string "Heatsink," *> sideParser)
          , FileFunction.Heatsinkmask
              <$> (Megaparsec.string "Heatsinkmask," *> maskParser)
          , FileFunction.Peelablemask
              <$> (Megaparsec.string "Peelablemask," *> maskParser)
          , FileFunction.Silvermask
              <$> (Megaparsec.string "Silvermask," *> maskParser)
          , FileFunction.Tinmask
              <$> (Megaparsec.string "Tinmask," *> maskParser)
          , FileFunction.Depthrout
              <$> (Megaparsec.string "Depthrout," *> sideParser)
          , FileFunction.Vcut
              <$> (Megaparsec.string "Vcut" *> optional (Megaparsec.char ',' *> sideParser))
          , FileFunction.Viafill <$ Megaparsec.string "Viafill"
          , fmap FileFunction.Pads $
              (Megaparsec.string "Pads," *> sideParser)
          , fmap FileFunction.Other $
              Megaparsec.string "Other" *> field
          , -- Drawing layers
            FileFunction.Drillmap <$ Megaparsec.string "Drillmap"
          , FileFunction.FabricationDrawing <$ Megaparsec.string "FabricationDrawing"
          , FileFunction.Vcutmap <$ Megaparsec.string "Vcutmap"
          , FileFunction.AssemblyDrawing
              <$> (Megaparsec.string "AssemblyDrawing," *> sideParser)
          , FileFunction.ArrayDrawing <$ Megaparsec.string "ArrayDrawing"
          , FileFunction.OtherDrawing
              <$> (Megaparsec.string "OtherDrawing" *> field)
          , FileFunction.KeepOut
              <$> (Megaparsec.string "Keep-out," *> sideParser)
          ]

    sideParser =
      asum $
        map
          Megaparsec.try
          [ Gerber.Side.Top <$ Megaparsec.string "Top"
          , Gerber.Side.Bottom <$ Megaparsec.string "Bot"
          ]

    markParser =
      asum $
        map
          Megaparsec.try
          [ Gerber.Top <$ Megaparsec.string "Top"
          , Gerber.Inner <$ Megaparsec.string "Inr"
          , Gerber.Bottom <$ Megaparsec.string "Bot"
          ]

    typeParser =
      asum $
        map
          Megaparsec.try
          [ Gerber.Plane <$ Megaparsec.string "Plane"
          , Gerber.Signal <$ Megaparsec.string "Signal"
          , Gerber.Mixed <$ Megaparsec.string "Mixed"
          , Gerber.Hatched <$ Megaparsec.string "Hatched"
          ]

    viaParser =
      asum $
        map
          Megaparsec.try
          [ Gerber.Drill.PTH <$ Megaparsec.string "PTH"
          , Gerber.Drill.NPTH <$ Megaparsec.string "NPTH"
          , Gerber.Drill.Blind <$ Megaparsec.string "Blind"
          , Gerber.Drill.Buried <$ Megaparsec.string "Buried"
          ]

    drillTypeParser =
      asum $
        map
          Megaparsec.try
          [ Gerber.Drill.DrillHole <$ Megaparsec.string "Drill"
          , Gerber.Drill.Route <$ Megaparsec.string "Rout"
          , Gerber.Drill.Mixed <$ Megaparsec.string "Mixed"
          ]

    profileParser =
      asum $
        map
          Megaparsec.try
          [ Gerber.Plated <$ Megaparsec.char 'P'
          , Gerber.NonPlated <$ Megaparsec.string "NP"
          ]

    maskParser =
      Gerber.Mask.Mask
        <$> sideParser
        <*> optional (Megaparsec.char ',' *> index)

    generationSoftware = do
      _ <- Megaparsec.string "GenerationSoftware"
      vendor <- field
      application <- field
      version <- optional field
      pure $ Gerber.Attribute.GenerationSoftware (Gerber.GenerationSoftware.GenerationSoftware{..})

    filePolarity = do
      _ <- Megaparsec.string "FilePolarity,"
      asum $
        map
          Megaparsec.try
          [ FilePolarity.Positive <$ Megaparsec.string "Positive"
          , FilePolarity.Negative <$ Megaparsec.string "Negative"
          ]

    sameCoordinates =
      Megaparsec.string "SameCoordinates" *> optional field


apertureAttribute :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Attribute.ApertureAttribute
apertureAttribute =
  asum $
    map
      Megaparsec.try
      [ aperFunctionParser
      , fmap Gerber.Attribute.DrillTolerance $
          Gerber.Attribute.Tolerance
            <$> (Megaparsec.string "DrillTolerance," *> float)
            <*> (Megaparsec.char ',' *> float)
      , flashTextParser
      ]
  where
    aperFunctionParser = do
      _ <- Megaparsec.string "AperFunction,"
      fmap Gerber.Attribute.AperFunction $
        asum $
          map
            Megaparsec.try
            [ viaDrill
            , AperFunction.BackDrill <$ Megaparsec.string "BackDrill"
            , componentDrill
            , mechanicalDrill
            , AperFunction.CastellatedDrill <$ Megaparsec.string "CastellatedDrill"
            , AperFunction.OtherDrill <$> (Megaparsec.string "OtherDrill," *> string)
            , AperFunction.ComponentPad <$ Megaparsec.string "ComponentPad"
            , AperFunction.SMDPad <$> (Megaparsec.string "SMDPad," *> padDefinitionParser)
            , AperFunction.BGAPad <$> (Megaparsec.string "BGAPad," *> padDefinitionParser)
            , AperFunction.ConnectorPad <$ Megaparsec.string "ConnectorPad"
            , AperFunction.HeatsinkPad <$ Megaparsec.string "HeatsinkPad"
            , AperFunction.ViaPad <$ Megaparsec.string "ViaPad"
            , AperFunction.TestPad <$ Megaparsec.string "TestPad"
            , AperFunction.CastellatedPad <$ Megaparsec.string "CastellatedPad"
            , AperFunction.FiducialPad <$> (Megaparsec.string "FiducialPad," *> fidSpecifierParser)
            , AperFunction.ThermalReliefPad <$ Megaparsec.string "ThermalReliefPad"
            , AperFunction.WasherPad <$ Megaparsec.string "WasherPad"
            , AperFunction.AntiPad <$ Megaparsec.string "AntiPad"
            , AperFunction.OtherPad <$> (Megaparsec.string "OtherPad," *> string)
            , AperFunction.Conductor <$ Megaparsec.string "Conductor"
            , AperFunction.EtchedComponent <$ Megaparsec.string "EtchedComponent"
            , AperFunction.NonConductor <$ Megaparsec.string "NonConductor"
            , AperFunction.CopperBalancing <$ Megaparsec.string "CopperBalancing"
            , AperFunction.Border <$ Megaparsec.string "Border"
            , AperFunction.OtherCopper <$> (Megaparsec.string "OtherCopper," *> string)
            , AperFunction.ComponentMain <$ Megaparsec.string "ComponentMain"
            , AperFunction.ComponentOutline <$> (Megaparsec.string "ComponentOutline," *> outlineTypeParser)
            , AperFunction.ComponentPin <$ Megaparsec.string "ComponentPin"
            , AperFunction.Profile <$ Megaparsec.string "Profile"
            , AperFunction.NonMaterial <$ Megaparsec.string "NonMaterial"
            , AperFunction.Material <$ Megaparsec.string "Material"
            , AperFunction.Other <$> (Megaparsec.string "Other," *> string)
            , Megaparsec.string "Drawing" *> string *> pure AperFunction.Drawing
            , Megaparsec.string "CutOut" *> string *> pure AperFunction.CutOut
            , Megaparsec.string "Slot" *> string *> pure AperFunction.Slot
            , Megaparsec.string "Cavity" *> string *> pure AperFunction.Cavity
            ]

    viaDrill = do
      _ <- Megaparsec.string "ViaDrill"
      ipc4761 <- optional (Megaparsec.char ',' *> ipc4761Parser)
      pure $ AperFunction.ViaDrill ipc4761

    ipc4761Parser =
      asum $
        map
          Megaparsec.try
          [ AperFunction.Ia <$ Megaparsec.string "Ia"
          , AperFunction.Ib <$ Megaparsec.string "Ib"
          , AperFunction.IIa <$ Megaparsec.string "IIa"
          , AperFunction.IIb <$ Megaparsec.string "IIb"
          , AperFunction.IIIa <$ Megaparsec.string "IIIa"
          , AperFunction.IIIb <$ Megaparsec.string "IIIb"
          , AperFunction.IVa <$ Megaparsec.string "IVa"
          , AperFunction.IVb <$ Megaparsec.string "IVb"
          , AperFunction.V <$ Megaparsec.string "V"
          , AperFunction.VI <$ Megaparsec.string "VI"
          , AperFunction.VII <$ Megaparsec.string "VII"
          , AperFunction.None <$ Megaparsec.string "None"
          , AperFunction.Deprecated AperFunction.Filled <$ Megaparsec.string "Filled"
          , AperFunction.Deprecated AperFunction.NotFilled <$ Megaparsec.string "NotFilled"
          ]

    componentDrill = do
      _ <- Megaparsec.string "ComponentDrill"
      pressfit <- optional $ (AperFunction.PressFit <$ Megaparsec.string ",PressFit")
      pure $ AperFunction.ComponentDrill pressfit

    mechanicalDrill = do
      _ <- Megaparsec.string "MechanicalDrill"
      purpose <-
        optional $
          asum $
            map
              Megaparsec.try
              [ AperFunction.Tooling <$ Megaparsec.string ",Tooling"
              , AperFunction.Breakout <$ Megaparsec.string ",Breakout"
              , AperFunction.OtherPurpose <$ Megaparsec.string ",Other"
              ]
      pure $ AperFunction.MechanicalDrill purpose

    padDefinitionParser =
      asum $
        map
          Megaparsec.try
          [ AperFunction.CuDef <$ Megaparsec.string "CuDef"
          , AperFunction.SMDef <$ Megaparsec.string "SMDef"
          ]

    fidSpecifierParser =
      asum $
        map
          Megaparsec.try
          [ AperFunction.Local <$ Megaparsec.string "Local"
          , AperFunction.Global <$ Megaparsec.string "Global"
          , AperFunction.Panel <$ Megaparsec.string "Panel"
          ]

    outlineTypeParser =
      asum $
        map
          Megaparsec.try
          [ AperFunction.Body <$ Megaparsec.string "Body"
          , AperFunction.Lead2Lead <$ Megaparsec.string "Lead2Lead"
          , AperFunction.Footprint <$ Megaparsec.string "Footprint"
          , AperFunction.Courtyard <$ Megaparsec.string "Courtyard"
          ]

    flashTextParser = do
      _ <- Megaparsec.string "FlashText,"
      text <- str
      _ <- Megaparsec.char ','
      bOrC <- asum $ map Megaparsec.try [Megaparsec.char 'B', Megaparsec.char 'C']
      _ <- Megaparsec.char ','
      rOrM <- optional $ asum $ map Megaparsec.try [Megaparsec.char 'R', Megaparsec.char 'M']
      _ <- Megaparsec.char ','
      font <- optional str
      _ <- Megaparsec.char ','
      fontSize <- optional int
      _ <- Megaparsec.char ','
      comment <- optional string
      pure $ Gerber.Attribute.FlashText{..}
      where
        str :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
        str = StrictText.pack <$> Megaparsec.some (Megaparsec.noneOf ['*', '%', ','])


objectAttributeName :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
objectAttributeName =
  asum $
    map
      Megaparsec.try
      [ Megaparsec.string ".N"
      , Megaparsec.string ".P"
      , Megaparsec.string ".CRot"
      , Megaparsec.string ".CMfr"
      , Megaparsec.string ".CMPN"
      , Megaparsec.string ".CVal"
      , Megaparsec.string ".CMnt"
      , Megaparsec.string ".CFtp"
      , Megaparsec.string ".CPgN"
      , Megaparsec.string ".CPgD"
      , Megaparsec.string ".CHgt"
      , Megaparsec.string ".CLbN"
      , Megaparsec.string ".CLbD"
      , Megaparsec.string ".CSup"
      , Megaparsec.string ".C" -- Placed after other options to prevent any ambiguity
      ]


ta :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ta = do
  _ <- Megaparsec.string "%TA."
  attr <- apertureAttribute
  _ <- Megaparsec.string "*%"
  newlines

  pure $ Gerber.TA attr


td :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
td =
  Megaparsec.string "%TD"
    *> (Gerber.TD <$> optional string)
    <* Megaparsec.string "*%"
    <* newlines


ln :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ln = do
  _ <- Megaparsec.string "%LN"
  name <- many (Megaparsec.noneOf ['*', '%'])
  _ <- Megaparsec.string "*%"
  newlines
  pure $ Gerber.LN (StrictText.pack name)


extendedCommands :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
extendedCommands =
  asum $
    map
      Megaparsec.try
      [ ad
      , am
      , fs
      , lp
      , mo
      , tf
      , to
      , ab
      , abEnd
      , lm
      , lr
      , ls
      , ta
      , td
      , sr
      , srEnd
      , ip
      , ln
      , of_
      , in_
      ]


deprecated :: Megaparsec.MonadParsec e StrictText.Text m => m [Gerber.Command]
deprecated = do
  g <-
    asum
      [ Gerber.G01 <$ (Megaparsec.string "G01" <|> Megaparsec.string "G1")
      , Gerber.G02 <$ (Megaparsec.string "G02" <|> Megaparsec.string "G2")
      , Gerber.G03 <$ (Megaparsec.string "G03" <|> Megaparsec.string "G3")
      ]

  dcode <-
    Megaparsec.try d01 <|> Megaparsec.try d02 <|> d03

  return [g, dcode]


commands :: Megaparsec.MonadParsec e StrictText.Text m => m [Gerber.Command]
commands =
  some command <|> deprecated


gerberFile :: Megaparsec.MonadParsec Void StrictText.Text m => m [Gerber.Command]
gerberFile =
  snoc
    <$> ( concat
            <$ many endOfBlock
            <*> many (commands <|> some extendedCommands)
        )
    <*> m02
    <* Megaparsec.eof
  where
    snoc xs x =
      xs ++ [x]


parseGerber ::
  StrictText.Text ->
  Either (Megaparsec.ParseErrorBundle StrictText.Text Void) [Gerber.Command]
parseGerber =
  Megaparsec.parse gerberFile "(gerber)"


parseGerberPretty ::
  StrictText.Text ->
  Either String [Gerber.Command]
parseGerberPretty =
  either (Left . Megaparsec.errorBundlePretty) Right . parseGerber
