{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Gerber.Grammar where

import Control.Applicative ( (<|>), many, optional, some )
import Control.Monad ( guard, void )
import Data.Char ( isAlphaNum, isDigit )
import Data.Foldable ( asum )
import Data.Monoid ( (<>) )
import Data.Void ( Void )
import Text.Megaparsec ( (<?>) )
import Text.Read ( readMaybe )

import qualified Data.Text as StrictText
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

import qualified Gerber.ApertureDefinition as Gerber
import qualified Gerber.Command as Gerber
import qualified Gerber.DCodeNumber as Gerber
import qualified Gerber.Format as Gerber
import qualified Gerber.Movement as Gerber
import qualified Gerber.Polarity as Gerber
import qualified Gerber.Unit as Gerber


digit :: Megaparsec.MonadParsec e StrictText.Text m => m Int
digit =
  read . pure <$> Megaparsec.satisfy isDigit <?> "0-9"


float :: Megaparsec.MonadParsec e StrictText.Text m => m Float
float = do
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
        fail "Invalid float"

      Just  a->
        return a


negative :: Num a => Megaparsec.MonadParsec e StrictText.Text m => m a -> m a
negative p = 
  asum [ negate <$ Megaparsec.char '-', pure id ]
    <*> p


int :: Megaparsec.MonadParsec e StrictText.Text m => m Int
int =
  read . StrictText.unpack <$> Megaparsec.takeWhile1P Nothing isDigit


string :: Megaparsec.MonadParsec e StrictText.Text m => m StrictText.Text
string =
  Megaparsec.takeWhile1P Nothing isStringChar


newlines :: Megaparsec.MonadParsec e StrictText.Text m => m ()
newlines = 
  void ( Megaparsec.takeWhileP Nothing ( `elem` ( "\n\r" :: [Char] ) ) )


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
fs = do
  Gerber.FS
    <$ Megaparsec.string "FSLA"
    <* Megaparsec.char 'X'
    <*> format
    <* Megaparsec.char 'Y'
    <*> format
    <* endOfBlock


mo :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
mo = do
  Gerber.MO
    <$ Megaparsec.string "MO"
    <*>
      ( ( Gerber.MM <$ Megaparsec.string "MM" )
        <|>
        ( Gerber.IN <$ Megaparsec.string "IN" )
      )
    <* endOfBlock


ad :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
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
    rectangleModifiers =
      Gerber.RectangleModifiers
        <$> float
        <* Megaparsec.char 'X'
        <*> float
        <*> optional ( Megaparsec.char 'X' *> float )

    rectangle =
      Gerber.Rectangle
        <$ Megaparsec.char 'R'
        <* Megaparsec.char ','
        <*> rectangleModifiers

    obround =
      Gerber.Obround
        <$ Megaparsec.char 'O'
        <* Megaparsec.char ','
        <*> rectangleModifiers

    polygon =
      error "TODO"
         <$ Megaparsec.char 'P'

    macro =
      Gerber.Macro
        <$> Megaparsec.takeWhile1P Nothing ( /= '*' )
   
   
  Gerber.AD
    <$ Megaparsec.string "AD"
    <* Megaparsec.char 'D'
    <*> dCodeNumber
    <*>
      asum
        [ Megaparsec.try circle
        , Megaparsec.try rectangle
        , Megaparsec.try obround
        , Megaparsec.try polygon
        , macro
        ]
    <* endOfBlock


g01 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g01 = do
  Gerber.G01
    <$ Megaparsec.string "G01"
    <* endOfBlock


g03 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g03 = do
  Gerber.G03
    <$ Megaparsec.string "G03"
    <* endOfBlock


g36 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g36 = do
  Gerber.G36
    <$ Megaparsec.string "G36"
    <* endOfBlock


g37 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g37 = do
  Gerber.G37
    <$ Megaparsec.string "G37"
    <* endOfBlock

g71 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g71 = do
  Gerber.G71
    <$ Megaparsec.string "G71"
    <* endOfBlock

g75 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g75 = do
  Gerber.G75
    <$ Megaparsec.string "G75"
    <* endOfBlock


m02 :: Megaparsec.MonadParsec e StrictText.Text m => m ()
m02 = do
  void ( Megaparsec.string "M02" )
    <* endOfBlock


g04 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
g04 = do
  Gerber.G04
    <$ ( Megaparsec.string "G04" <|> Megaparsec.string "LN" )
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
d = do
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
    <$> optional ( Megaparsec.char 'X' *> negative int )
    <*> optional ( Megaparsec.char 'Y' *> negative int )
    <*> optional ( Megaparsec.char 'I' *> negative int )
    <*> optional ( Megaparsec.char 'J' *> negative int )


d01 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d01 =
    Gerber.D01
      <$> movement
      <* Megaparsec.string "D01"
      <* endOfBlock


d02 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d02 =
    Gerber.D02
      <$> movement
      <* Megaparsec.string "D02"
      <* endOfBlock


d03 :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
d03 =
    Gerber.D03
      <$> movement
      <* Megaparsec.string "D03"
      <* endOfBlock

am :: Megaparsec.MonadParsec e StrictText.Text m => m Gerber.Command
am =
    Gerber.AM
      <$ Megaparsec.string "AM"
      <* Megaparsec.someTill Megaparsec.anyChar endOfBlock
      <* Megaparsec.manyTill Megaparsec.anyChar ( Megaparsec.lookAhead ( Megaparsec.char '%' ) )


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
        , g75
        , g03
        , g71
        , of_
        , am
        ]
    )

deprecated :: Megaparsec.MonadParsec e StrictText.Text m => m [Gerber.Command]
deprecated = do
  g <-
    asum
      [ Gerber.G01 <$ Megaparsec.string "G01"
      , Gerber.G02 <$ Megaparsec.string "G02"
      , Gerber.G03 <$ Megaparsec.string "G03"
      ]

  d <- d01 <|> d02

  return [ g, d ]


extended :: Megaparsec.MonadParsec e StrictText.Text m => m a -> m [a]
extended command =
  Megaparsec.char '%'
    *> some command
    <* Megaparsec.char '%'
    <* newlines


commands :: Megaparsec.MonadParsec e StrictText.Text m => m [ Gerber.Command ]
commands =
  some command <|> deprecated


gerberFile :: Megaparsec.MonadParsec Void StrictText.Text m => m [ Gerber.Command ]
gerberFile =
  concat
    <$ many endOfBlock
    <*> many ( commands <|> ( concat <$> extended commands ) )
    <* m02
    <* Megaparsec.eof


isStringChar :: Char -> Bool
isStringChar c =
  isAlphaNum c
    || c `elem` ( "_+-/!?<>”’(){}.\\|&@# ,;$:=" :: [Char] )
