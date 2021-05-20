{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}


module Gerber.Grammar ( parseGerber ) where

import Control.Applicative ( (<|>), empty, many, optional, some )
import Control.Monad ( guard, void )
import Data.Char ( digitToInt, isDigit )
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable ( asum )
import Data.Void ( Void )
import Text.Megaparsec ( (<?>) )
import Text.Read ( readMaybe )
import Linear.V2 (V2(..))

import qualified Data.Text as StrictText
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Megaparsec

import qualified Gerber.Padding as Padding
import qualified Gerber.ApertureDefinition as Gerber
import Gerber.MacroDefinition as MacroDefinition
import qualified Gerber.Command as Gerber
import qualified Gerber.DCodeNumber as Gerber
import qualified Gerber.EncodedDecimal as Gerber
import qualified Gerber.Format as Gerber
import qualified Gerber.Mirroring as Gerber
import qualified Gerber.Movement as Gerber
import qualified Gerber.Polarity as Gerber
import qualified Gerber.StepRepeat as Gerber
import qualified Gerber.Unit as Gerber


digit :: Megaparsec.MonadParsec e StrictText.Text m => m Int
digit =
  read . pure <$> Megaparsec.satisfy isDigit <?> "0-9"


float :: Megaparsec.MonadParsec e StrictText.Text m => m Float
float = do
  negated <- maybe id ( const negate ) <$> optional ( Megaparsec.char '-' )

  intPart <-
    Megaparsec.takeWhile1P Nothing isDigit

  decPart <- optional $ do
    void ( Megaparsec.string "." )

    Megaparsec.takeWhile1P Nothing isDigit

  case
    readMaybe
      ( StrictText.unpack ( intPart <> maybe "" ( "." <> ) decPart ) )
    of
      Nothing ->
        empty

      Just a ->
        pure $ negated a


negative :: Num a => Megaparsec.MonadParsec e StrictText.Text m => m a -> m a
negative p =
  asum [ negate <$ Megaparsec.char '-', pure id ]
    <*> p


int :: Megaparsec.MonadParsec e StrictText.Text m => m Int
int =
  read . StrictText.unpack <$> Megaparsec.takeWhile1P Nothing isDigit


string :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
string =
  Megaparsec.takeWhileP Nothing isStringChar


newlines :: Megaparsec.MonadParsec e StrictText.Text m => m ()
newlines =
  void ( Megaparsec.takeWhileP Nothing ( `elem` ( "\n\r" :: String ) ) )


endOfBlock :: Megaparsec.MonadParsec e StrictText.Text m => m ()
endOfBlock = do
  _ <-
    Megaparsec.char '*'

  newlines

  return ()


format :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Format
format = do
  integerPositions <-
    digit

  decimalPositions <-
    digit

  return Gerber.Format{..}


fs :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
fs =
  Gerber.FS
    <$> asum
          [ Padding.PadLeading <$ Megaparsec.string "FSLA"
          , Padding.PadTrailing <$ Megaparsec.string "FSTA"
          ]
    <* Megaparsec.char 'X'
    <*> format
    <* Megaparsec.char 'Y'
    <*> format
    <* endOfBlock


mo :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
mo =
  Gerber.MO
    <$ Megaparsec.string "MO"
    <*>
      ( ( Gerber.MM <$ Megaparsec.string "MM" )
        <|>
        ( Gerber.IN <$ Megaparsec.string "IN" )
      )
    <* endOfBlock


ad :: forall e m. Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ad = do
  let
    dCodeNumber =
      Gerber.DCodeNumber <$> int

    circle =
      Gerber.Circle
        <$ Megaparsec.char 'C'
        <* Megaparsec.char ','
        <*> ( Gerber.CircleModifiers
                <$> float
                <*> optional ( Megaparsec.char 'X' *> float )
            )

    polygonModifiers =
      Gerber.PolygonModifiers
        <$> float
        <* Megaparsec.char 'X'
        <*> int
        <*> optional ( Megaparsec.char 'X' *> float )
        <*> optional ( Megaparsec.char 'X' *> float )

    rectangleModifiers =
      Gerber.RectangleModifiers
        <$> float
        <* Megaparsec.char 'X'
        <*> float
        <*> optional ( Megaparsec.char 'X' *> float )

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
        Gerber.MacroAD n
           <$> Megaparsec.takeWhile1P Nothing ( `notElem` [',', '*'] )
           <* optional ( Megaparsec.char ',' )
           <*> ( Megaparsec.sepBy float ( Megaparsec.char 'X' ) )

  n <- Megaparsec.string "AD" >> Megaparsec.char 'D' >> dCodeNumber
  let ad' = Megaparsec.try . fmap ( Gerber.AD n )
  asum
    [ ad' circle
    , ad' rectangle
    , ad' obround
    , ad' polygon
    , macro n
    ]
    <* endOfBlock


g01 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g01 =
  Gerber.G01
    <$ Megaparsec.string "G01"
    <* endOfBlock


g03 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g03 =
  Gerber.G03
    <$ Megaparsec.string "G03"
    <* endOfBlock


g36 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g36 =
  Gerber.G36
    <$ Megaparsec.string "G36"
    <* endOfBlock


g37 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g37 =
  Gerber.G37
    <$ Megaparsec.string "G37"
    <* endOfBlock

g71 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g71 =
  Gerber.G71
    <$ Megaparsec.string "G71"
    <* endOfBlock


g74 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g74 =
  Gerber.G74
    <$ Megaparsec.string "G74"
    <* endOfBlock


g75 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g75 =
  Gerber.G75
    <$ Megaparsec.string "G75"
    <* endOfBlock


m02 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
m02 =
  Gerber.M02
    <$ Megaparsec.string "M02"
    <* endOfBlock


g04 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g04 =
  Gerber.G04
    <$ ( Megaparsec.string "G04" <|> Megaparsec.string "LN" <|> Megaparsec.string "IN" )
    <*> string
    <* endOfBlock


lp :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
lp =
  Gerber.LP
    <$ Megaparsec.string "LP"
    <*> polarity
    <* endOfBlock


polarity :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Polarity
polarity =
  asum [ Gerber.Clear <$ Megaparsec.char 'C'
       , Gerber.Dark <$ Megaparsec.char 'D'
       ]


d :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d =
  Gerber.D
    <$ optional ( Megaparsec.string "G54" )
    <* Megaparsec.char 'D'
    <*>
      ( do
          n <-
            read . StrictText.unpack
              <$>
                ( StrictText.cons
                    <$> Megaparsec.satisfy isDigit
                    <*> Megaparsec.takeWhile1P Nothing isDigit
                )

          guard ( n >= 10 )

          return  ( Gerber.DCodeNumber n )
      )
    <* endOfBlock

movement :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Movement
movement =
  Gerber.Movement
    <$> optional ( Megaparsec.char 'X' *> encodedDecimal )
    <*> optional ( Megaparsec.char 'Y' *> encodedDecimal )
    <*> optional ( Megaparsec.char 'I' *> encodedDecimal )
    <*> optional ( Megaparsec.char 'J' *> encodedDecimal )

  where

    encodedDecimal = do
      negative <-
        asum
          [ True <$ Megaparsec.char '-'
          , pure False
          ]

      digits <-
        map digitToInt . StrictText.unpack <$> Megaparsec.takeWhileP Nothing isDigit

      return Gerber.EncodedDecimal{..}


stepRepeat :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.StepRepeat
stepRepeat =
  Gerber.StepRepeat
    <$> ( Megaparsec.char 'X' *> int )
    <*> ( Megaparsec.char 'Y' *> int )
    <*> ( Megaparsec.char 'I' *> negative float )
    <*> ( Megaparsec.char 'J' *> negative float )


d01 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d01 =
    Gerber.D01
      <$> movement
      <* optional ( Megaparsec.string "D01" <|> Megaparsec.string "D1" )
      <* endOfBlock


d02 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d02 =
    Gerber.D02
      <$> movement
      <* ( Megaparsec.string "D02" <|> Megaparsec.string "D2"  )
      <* endOfBlock


d03 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d03 =
    Gerber.D03
      <$> movement
      <* ( Megaparsec.string "D03" <|> Megaparsec.string "D3"  )
      <* endOfBlock

am :: forall e m. Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
am = Gerber.AM
  <$ ( Megaparsec.string "AM" )
  <*> ( StrictText.pack <$> Megaparsec.someTill Megaparsec.anySingle endOfBlock' )
  <*> ( Megaparsec.sepEndBy content endOfBlock' )
  where
    endOfBlock' :: m ()
    endOfBlock' = endOfBlock >> newlines

    content :: m ( MacroDefinition.Definition MacroDefinition.Modifier MacroDefinition.Modifier )
    content =
      ( asum . map Megaparsec.try )
        [ MacroDefinition.Variable <$> ( Megaparsec.char '$' >> Megaparsec.decimal <* Megaparsec.char '=' ) <*> modifier
        , MacroDefinition.Comment <$ checkCode 0 <*> ( Megaparsec.takeWhileP Nothing ( /='*' ) )
        , MacroDefinition.Primitive . MacroDefinition.Circle <$> circle
        , MacroDefinition.Primitive . MacroDefinition.VectorLine <$> vectorLine
        , MacroDefinition.Primitive . MacroDefinition.CenterLine <$> centerLine
        , MacroDefinition.Primitive . MacroDefinition.Outline <$> outline
        , MacroDefinition.Primitive . MacroDefinition.Polygon <$> polygon
        , MacroDefinition.Primitive . MacroDefinition.Moire <$> moire
        , MacroDefinition.Primitive . MacroDefinition.Thermal <$> thermal
        ]
      where

        checkCode :: Int -> m ()
        checkCode code = Megaparsec.decimal >>= \a -> guard ( a == code )

        comma :: m ()
        comma = void ( Megaparsec.char ',' >> newlines )

        coordinatePair :: m ( V2 MacroDefinition.Modifier )
        coordinatePair = V2 <$> ( modifier <* comma ) <*> modifier

        circle :: m ( MacroDefinition.CircleModifiers MacroDefinition.Modifier MacroDefinition.Modifier )
        circle =
          MacroDefinition.CircleModifiers
            <$ ( checkCode 1 <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> coordinatePair
            <*> optional ( comma >> modifier )

        vectorLine :: m ( MacroDefinition.VectorLineModifiers MacroDefinition.Modifier MacroDefinition.Modifier )
        vectorLine =
          MacroDefinition.VectorLineModifiers
            <$ ( checkCode 20 <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( coordinatePair  <* comma )
            <*> ( coordinatePair  <* comma )
            <*> modifier

        centerLine :: m ( MacroDefinition.CenterLineModifiers MacroDefinition.Modifier MacroDefinition.Modifier )
        centerLine =
          MacroDefinition.CenterLineModifiers
            <$ ( checkCode 21 <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( coordinatePair  <* comma )
            <*> modifier

        outline :: m ( MacroDefinition.OutlineModifiers MacroDefinition.Modifier MacroDefinition.Modifier )
        outline =
          MacroDefinition.OutlineModifiers
            <$ ( checkCode 4 <* comma )
            <*> ( modifier <* comma )
            <*> ( vertices <* comma )
            <*> modifier
          where
            vertices = do
              -- from the spec The number of vertices of the outline = the number of coordinate pairs minus one.  An integer ≥3.
              numVertices <- Megaparsec.decimal <* comma
              sequence . ( coordinatePair :| ) . replicate numVertices $ ( comma >> coordinatePair )

        polygon :: m ( MacroDefinition.PolygonModifiers MacroDefinition.Modifier MacroDefinition.Modifier )
        polygon =
          MacroDefinition.PolygonModifiers
            <$ ( checkCode 5 <* comma )
            <*> ( modifier <* comma )
            <*> ( Megaparsec.decimal <* comma )
            <*> ( coordinatePair <* comma )
            <*> ( modifier <* comma )
            <*> modifier

        moire :: m ( MacroDefinition.MoireModifiers MacroDefinition.Modifier )
        moire =
          MacroDefinition.MoireModifiers
            <$ ( checkCode 6 <* comma )
            <*> ( coordinatePair <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> modifier

        thermal :: m ( MacroDefinition.ThermalModifiers MacroDefinition.Modifier )
        thermal =
          MacroDefinition.ThermalModifiers
            <$ ( checkCode 7 <* comma )
            <*> ( coordinatePair <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> ( modifier <* comma )
            <*> modifier

        modifier :: m ( MacroDefinition.Modifier )
        modifier = fixAssociation <$> unaryOrBinaryModifier
          where
            unaryOrBinaryModifier :: m MacroDefinition.Modifier
            unaryOrBinaryModifier = do
              a <- asum $ map Megaparsec.try
                [ unaryModifier
                , UnaryPlus <$ Megaparsec.char '+' <*> unaryModifier
                , UnaryMinus <$ Megaparsec.char '-' <*> unaryModifier
                ]
              binayrModifier a <|> pure a

            unaryModifier :: m MacroDefinition.Modifier
            unaryModifier = asum $ map Megaparsec.try
              [ Megaparsec.between ( Megaparsec.char '(' ) ( Megaparsec.char ')' ) ( Parentheses <$> unaryOrBinaryModifier )
              , Decimal <$> float
              , VariableReference <$ Megaparsec.char '$' <*> Megaparsec.decimal
              ]

            binayrModifier :: Modifier -> m MacroDefinition.Modifier
            binayrModifier m = asum $ map Megaparsec.try
              [ Plus m <$ Megaparsec.char '+' <*> unaryOrBinaryModifier
              , Minus m <$ Megaparsec.char '-' <*> unaryOrBinaryModifier
              , Multiply m <$ Megaparsec.satisfy (`elem` ['x','X'])  <*> unaryOrBinaryModifier
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
                    Parentheses ( go x )

                  UnaryPlus x ->
                    UnaryPlus ( go x )

                  UnaryMinus x ->
                    UnaryMinus ( go x )

                  Plus l r ->
                    Plus ( go l ) ( go r )

                  Minus l r ->
                    case go r of
                      Plus rl rr ->
                        Plus ( Minus ( go l ) rl ) rr
                      _ ->
                        Minus ( go l ) ( go r )

                  Multiply l r ->
                    case go r of
                      Plus rl rr ->
                        Plus ( Multiply ( go l ) rl ) rr

                      Minus rl rr ->
                        Minus ( Multiply ( go l ) rl ) rr

                      _ ->
                        Multiply ( go l ) ( go r )

                  Divide l r ->
                    case go r of
                      Plus rl rr ->
                        Plus ( Divide ( go l ) rl ) rr

                      Minus rl rr ->
                        Minus ( Divide ( go l ) rl ) rr

                      Multiply rl rr ->
                        Multiply ( Divide ( go l ) rl ) rr

                      _ ->
                        Divide ( go l ) ( go r )


sr :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
sr =
    Gerber.SR
      <$  Megaparsec.string "SR"
      <*> stepRepeat
      <* endOfBlock

srEnd :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
srEnd =
    Gerber.SR_End
      <$  Megaparsec.string "SR"
      <* endOfBlock

sf :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
sf =
    Gerber.SF
      <$ Megaparsec.string "SF"
      <* optional ( Megaparsec.string "A" *> float >>= guard . ( == 1 ) )
      <* optional ( Megaparsec.string "B" *> float >>= guard . ( == 1 ) )
      <* endOfBlock


mi :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
mi =
    Gerber.MI
      <$ Megaparsec.string "MI"
      <* optional ( Megaparsec.string "A" *> Megaparsec.string "0" )
      <* optional ( Megaparsec.string "B" *> Megaparsec.string "0" )
      <* endOfBlock


of_ :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
of_ =
    Gerber.OF
      <$ Megaparsec.string "OF"
      <*> optional ( Megaparsec.string "A" *> float )
      <*> optional ( Megaparsec.string "B" *> float )
      <* endOfBlock


ip :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ip =
    Gerber.IP
      <$ Megaparsec.string "IP"
      <* ( Megaparsec.string "POS" <|> Megaparsec.string "NEG" )
      <* endOfBlock

ab :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ab =
    Gerber.AB
      <$ Megaparsec.string "ABD"
      <*> ( Gerber.DCodeNumber <$> int )
      <* endOfBlock

abEnd :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
abEnd =
    Gerber.AB_End
      <$ Megaparsec.string "AB"
      <* endOfBlock

lm :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
lm =
    Gerber.LM
      <$ Megaparsec.string "LM"
      <*> ( Megaparsec.choice . map Megaparsec.try )
            [ Gerber.MirrorXY <$ Megaparsec.string "XY"
            , Gerber.MirrorNone <$ Megaparsec.char 'N'
            , Gerber.MirrorX <$ Megaparsec.char 'X'
            , Gerber.MirrorY <$ Megaparsec.char 'Y'
            ]
      <* endOfBlock

lr :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
lr =
    Gerber.LR
      <$ Megaparsec.string "LR"
      <*> float
      <* endOfBlock

ls :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ls =
    Gerber.LS
      <$ Megaparsec.string "LS"
      <*> float
      <* endOfBlock

command :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
command =
  asum
    ( map Megaparsec.try
        [ fs
        , mo
        , ad
        , g01
        , g04
        , lp
        , d
        , d01
        , d02
        , d03
        , g36
        , g37
        , ip
        , g74
        , g75
        , g03
        , g71
        , of_
        , am
        , sr
        , srEnd
        , sf
        , mi
        , ingoredAttribute
        , ab
        , abEnd
        , lm
        , lr
        , ls
        ]
    )

ingoredAttribute :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
ingoredAttribute = asum $ map Megaparsec.try
  [ mk Gerber.IngoredAttributeTF "TF."
  , mk Gerber.IngoredAttributeTA "TA."
  , mk Gerber.IngoredAttributeTO "TO."
  , mk Gerber.IngoredAttributeTD "TD."
  ]
  where
    mk constructor prefix  =
      constructor
        <$ Megaparsec.string prefix
        <*> ( Megaparsec.takeWhile1P Nothing (/= '*') )
        <* endOfBlock

deprecated :: Megaparsec.MonadParsec e StrictText.Text m => m [Gerber.Command]
deprecated = do
  g <-
    asum
      [ Gerber.G01 <$ ( Megaparsec.string "G01" <|> Megaparsec.string "G1" )
      , Gerber.G02 <$ ( Megaparsec.string "G02" <|> Megaparsec.string "G2" )
      , Gerber.G03 <$ ( Megaparsec.string "G03" <|> Megaparsec.string "G3" )
      ]

  dcode <-
    Megaparsec.try d01 <|> Megaparsec.try d02 <|> d03

  return [ g, dcode ]


extended :: Megaparsec.MonadParsec e StrictText.Text m => m a -> m [a]
extended parser =
  Megaparsec.char '%'
    *> some parser
    <* Megaparsec.char '%'
    <* newlines


commands :: Megaparsec.MonadParsec e StrictText.Text m => m [ Gerber.Command ]
commands =
  some command <|> deprecated


gerberFile :: Megaparsec.MonadParsec Void StrictText.Text m => m [ Gerber.Command ]
gerberFile =
  snoc
    <$> ( concat
            <$ many endOfBlock
            <*> many ( commands <|> ( concat <$> extended commands ) )
        )
    <*> m02
    <* Megaparsec.eof

  where

    snoc xs x =
      xs ++ [x]


isStringChar :: Char -> Bool
isStringChar c =
  c `notElem` ( "\n\r%*" :: String )


parseGerber
  :: StrictText.Text
  -> Either (Megaparsec.ParseErrorBundle StrictText.Text Void) [Gerber.Command]
parseGerber =
  Megaparsec.parse gerberFile "(gerber)"
