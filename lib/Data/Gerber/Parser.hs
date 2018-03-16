{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Data.Gerber.Parser
--
-- Defines parsers for parsing gerber files to AST defined in "Data.Gerber.Types" according to
-- to specification https://www.ucamco.com/files/downloads/file/81/the_gerber_file_format_specification.pdf .
--
-- Deviation Notes
-- ================
-- * According to the specification if you strip all eol characters it is still a valid file.
--   I am assuming this is not actually really intended and that most writers don't do this.
--   So eol's are only stripped outside of blocks.
-- * Technically according to the specification all parameters to a macro primitive can be
--   based on a macor modifier, that is be the result of parametric expressions.
--   * One issue for the polygon primitive is that if the vertix count is polymorphic then
--     the primitive is not parsable so its assumed to be constant.
--   * Another assumption is that exposure for primitives can only be on off or simple
--     variable substitution.
--   * Essentially macro modifiers are parametric double expressions so the macro template
--     specification is ill defined when the result has to be an integer or boolean or where the
--     result would influence subsequent parsing.
--
-- Parsing Notes
-- ====================
-- * Parser orignially written for attoparsec then switched to megaprsec.
-- * In Attoparsec all parsers are backtracking (no need for try) which is not the case for megaparsec,
--   so all the top level parsers were made backtracking
-- * Megaparsec string and char parsers are backtracking / does not consume any imput so does not need try
-- * In Megaparsec for failure cases lookAhead needs try
-- * Note this is a lot slower than the attoparsec version but does not rely on the custom lifted
--   attoparsec cominators
-- * The parser should probably be rewritten for speed and better error messages currently
--   when it fails it just indicats the top level "gerberCommand" parser failed.

module Data.Gerber.Parser where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Gerber.Types
import Data.Monoid
import Data.Void (Void)
import Text.Megaparsec hiding (region)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

-- | Context used in our parser
data Context = Context
  { inExtended :: Bool -- ^ Inside an extended command
  , activeCoordinateFormat :: Maybe CoordinateFormat -- ^ The coordinate format has been defined
  } deriving (Show)

lInExtended :: Lens' Context Bool
lInExtended =
  lens
    inExtended
    (\c b ->
        c
        { inExtended = b
        })

lActiveCoordinateFormat :: Lens' Context (Maybe CoordinateFormat)
lActiveCoordinateFormat =
  lens
    activeCoordinateFormat
    (\c b ->
        c
        { activeCoordinateFormat = b
        })

initialContext :: Context
initialContext =
  Context
  { inExtended = False
  , activeCoordinateFormat = Nothing
  }

type Parser a = ReaderT Context (ParsecT Void Text Identity) a


takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = T.pack <$> many (satisfy p)

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = T.pack <$> some (satisfy p)

double :: Parser Double
double = try $ label "double" $ signed space (try float <|> fmap fromIntegral decimal)

decimalInt :: Parser Int
decimalInt = fromIntegral <$> decimal

blockEnd :: Parser ()
blockEnd = try $
  label "block end" $
  do b <- view lInExtended
     if b
       then endInExtend
       else blockOnlyEnd
  where
    endInExtend = lookAhead (try extendEnd) <|> blockOnlyEnd
    blockOnlyEnd = char '*' >> skipMany eol


extendStart :: Parser b -> Parser b
extendStart p = try $
  label "extended command start" $
  do view (lInExtended . to not) >>= guard
     void (char '%' >> lookAhead (count 2 $ satisfy isAlpha_ascii))
     local (set lInExtended True) p

extendEnd :: Parser ()
extendEnd = try $ label "extended command end" (char '*' >> skipMany eol >> char '%' >> skipMany eol)


extended :: Parser a -> Parser a
extended p = extendStart (p <* extendEnd)

extendedString :: Text -> Parser Text
extendedString = extended . string

name :: Parser Name
name = try $ label "name" $
  Name <$>
  liftA2
    T.cons
    (satisfy isNameFirst <?> "first")
    (takeWhile isNameNotFirst <?> "not first")

field :: Parser Field
field = try $ Field <$> takeWhile1 isField

stringContent :: Parser StringContent
stringContent = try $ StringContent <$> takeWhile1 isStringContent

data FCode
  = G Int
  | D Int
  | M Int
  deriving (Show)

fCode :: Parser FCode
fCode = try $ label "fCode" $
  ((G <$ char 'G') <|> (D <$ char 'D') <|> (M <$ char 'M')) <*> decimalInt

fixedPrecisionEncoded :: Parser Double
fixedPrecisionEncoded =
  try $ label "FixedPrecisionEncoded" $ signed (pure ()) $
  do cdFmt <-
       view lActiveCoordinateFormat >>=
       maybe (fail "Coordinate format has not been set yet") pure
     bs <-  takeWhile1 isDigit
     maybe (fail "Could not parse double") pure (decodeFixedPrecision cdFmt bs)

decodeFixedPrecision :: CoordinateFormat -> Text -> Maybe Double
decodeFixedPrecision CoordinateFormat {..} bs =
  let il = T.length bs
      rl = max coordinateFormatDecimals 0 + max coordinateFormatIntegers 0
      (bsI, bsD) =
        T.splitAt
          coordinateFormatIntegers
          (T.replicate (rl - il) "0" <> T.drop (il - rl) bs)
      prs :: Parsec Void Text Double
      prs = float <* eof
  in parseMaybe prs (T.cons '0' bsI <> T.cons '.' bsD)

coordinateModify :: Parser CoordinateModify
coordinateModify =
  try $ label "CoordinateModify" $
  CoordinateModify <$>
  ((char 'X' >> fmap Just fixedPrecisionEncoded) <|> pure Nothing) <*>
  ((char 'Y' >> fmap Just fixedPrecisionEncoded) <|> pure Nothing)

circularOffset :: Parser CircularOffset
circularOffset = try $ label "CircularOffset" $
  CircularOffset <$> (char 'I' >> fixedPrecisionEncoded) <*>
  (char 'J' >> fixedPrecisionEncoded)

interpolateTo :: Parser InterpolateTo
interpolateTo =
  try $ label "InterpolateTo" $
  do crd <- coordinateModify
     moff <- fmap Just circularOffset <|> pure Nothing
     code <- fCode <* blockEnd
     case code of
       D 1 -> pure (InterpolateTo crd moff)
       _ -> fail ("Received unknown function code " ++ show code)

moveTo :: Parser MoveTo
moveTo =
  try $ label "MoveTo" $
  do crd <- coordinateModify
     code <- fCode <* blockEnd
     case code of
       D 2 -> pure (MoveTo crd)
       _ -> fail ("Received unknown function code " ++ show code)

flashTo :: Parser FlashTo
flashTo =
  try $ label "flashTo" $
  do try deprecatedPfx <|> pure ()
     crd <- coordinateModify
     code <- fCode <* blockEnd
     case code of
       D 3 -> pure (FlashTo crd)
       _ -> fail ("Received unknown function code " ++ show code)
  where
    deprecatedPfx = void (string "G55")

setAperature :: Parser SetAperature
setAperature = SetAperature <$> ((try deprecatedPfx <|> pure ()) >> apperaturId <* blockEnd)
  where
    deprecatedPfx = void $ string "G54"

graphicsState :: Parser GraphicsState
graphicsState =
  try $ label "GraphicsState" $
  do code <- fCode <* blockEnd
     case code of
       G 1 -> pure SetInterpolationLinear
       G 2 -> pure SetInterpolationCircularClockWise
       G 3 -> pure SetInterpolationCircularCounterClockWise
       G 74 -> pure SetInterpolationCircularSingleQuadrant
       G 75 -> pure SetInterpolationCircularMultiQuadrant
       _ -> fail ("Received unknown function code " ++ show code)

graphicsStateDeprecatedLegacy :: Parser GraphicsState
graphicsStateDeprecatedLegacy =
  try $ label "GraphicsState" $
  do code <- fCode <* lookAhead (try regionCommand)
     case code of
       G 1 -> pure SetInterpolationLinear
       G 2 -> pure SetInterpolationCircularClockWise
       G 3 -> pure SetInterpolationCircularCounterClockWise
       _ -> fail ("Received unknown function code " ++ show code)

regionCommand :: Parser RegionCommand
regionCommand =
  choice
    [ InterpolateToRc <$> interpolateTo
    , MoveToRc <$> moveTo
    , StateRc <$> (graphicsState <|> graphicsStateDeprecatedLegacy)
    , CommentRc <$> comment
    ] <?>
  "RegionCommand"

region :: Parser Region
region = try $ label "Region" $ g 36 *> (Region <$> manyTill regionCommand (g 37))
  where
    g :: Int -> Parser ()
    g i = try $ char 'G' >> decimalInt >>= guard . (== i) >> blockEnd

comment :: Parser Comment
comment =
  try $ label "Comment" $
  do code <- fCode
     cnt <- stringContent
     blockEnd
     case code of
       G 4 -> pure (Comment cnt)
       _ -> fail ("Received unknown function code " ++ show code)

endOfFileCode :: Parser EndOfFileCode
endOfFileCode =
  try $ label "EndOfFileCode" $
  do code <- fCode
     blockEnd
     case code of
       M 2 -> pure EndOfFileCode
       _ -> fail ("Received unknown function code " ++ show code)

coordinateFormat :: Parser CoordinateFormat
coordinateFormat =
  try $ label "CoordinateFormat" $
  extendStart $
  do void (string "FSLAX")
     xi <- read . (: []) <$> satisfy isDigit <?> "X integer length"
     xd <- read . (: []) <$> satisfy isDigit <?> "X decimal length"
     void (char 'Y')
     yi <- read . (: []) <$> satisfy isDigit <?> "Y integer length"
     yd <- read . (: []) <$> satisfy isDigit <?> "Y decimal length"
     extendEnd
     if xi == yi && xd == yd && xi >= 0 && xi <= 6 && xd >= 4 && xd <= 6
       then pure (CoordinateFormat xi xd)
       else fail
              ("Expecting XNMYNM with 0 <= N <= 6 and 4 <= M <= 6, got X" ++
               show xi ++ show xd ++ "Y" ++ show yi ++ show yd)

units :: Parser Units
units =
  (Inches <$ extendedString "MOIN") <|> (Milimeters <$ extendedString "MOMM") <|>
  deprecatedUnits <?> "Units"
  where
    deprecatedUnits =
      try $
      deprecatedCommand >>=
      \c ->
         case c of
           SetUnitInchDc -> pure Inches
           SetUnitMmDc -> pure Milimeters
           _ -> mzero

apperaturId :: Parser AperaturId
apperaturId =
  try $ label "AperaturId" $
  do code <- fCode
     case code of
       D a
         | a >= 10 && a <= 2147483647 -> pure (AperaturId a)
         | otherwise ->
           fail
             ("Aperature ID must be greater than 10 and less than 2147483647 got " ++
              show a)
       _ -> fail ("Received unknown function code " ++ show code)

aperatureDef :: Parser AperatureDef
aperatureDef = try $ extendStart (void (string "AD") *> (AperatureDef <$> apperaturId <*> adType_) <* extendEnd)
  where
    modifierFirst_ = char ',' >> double
    modifier_ = char 'X' >> double
    modifierInt_ = char 'X' >> decimalInt
    modifierMaybe_ = fmap Just (try modifier_) <|> pure Nothing
    --------
    circleStroke_ = try $ char 'C' >> (CircleStrokeAd <$> modifierFirst_) <* blockEnd
    rectangleStroke_ = try $ char 'R' >> (RectangleStrokeAd <$> modifierFirst_ <*> modifier_) <* blockEnd
    circle_ = try $ char 'C' >> (CircleStandardAd <$> modifierFirst_ <*> modifier_) <* blockEnd
    rectangle_ = try $ char 'R' >> (RectangleStandardAd <$> modifierFirst_ <*> modifier_ <*> modifier_) <* blockEnd
    obround_ = try $ char 'O' >> (ObroundStandardAd <$> modifierFirst_ <*> modifier_ <*> modifierMaybe_) <* blockEnd
    polygon_ = try $
      char 'P' >>
      (PolygonStandardAd <$> modifierFirst_ <*> modifierInt_ <*> modifierMaybe_ <*> modifierMaybe_) <* blockEnd
    --------
    strokeAd_ = circleStroke_ <|> rectangleStroke_
    standardAd_ = circle_ <|> rectangle_ <|> obround_ <|> polygon_
    modifiers_ = ((:) <$> modifierFirst_ <*> many modifier_) <|> pure []
    macroAd_ = MacroAd <$> name <*> modifiers_
    adType_ =
      (StrokeAdt <$> strokeAd_) <|>
      (StandardAdt <$> standardAd_) <|>
      (MacroAdt <$> macroAd_)

macroModifier :: Parser MacroModifier
macroModifier = try start_
  where
    start_ :: Parser MacroModifier
    start_ = do
      space
      s <- negate_ <*> (paren_ <|> leaf_)
      n <- try next_ <|> pure id
      pure (n s)
    next_ :: Parser (MacroModifier -> MacroModifier)
    next_ = do
      space
      o <- oneOf ("-+x/" :: String) <|> legacyAllowCapitalX
      r <- start_
      pure $
        case (o, r) of
          ('+', _) -> (`AddMm` r)
          ('-', _) -> (`SubMm` r)
          ('x', AddMm l' r') -> (`AddMm` r') . (`MulMm` l')
          ('x', SubMm l' r') -> (`SubMm` r') . (`MulMm` l')
          ('x', _) -> (`MulMm` r)
          ('/', AddMm l' r') -> (`AddMm` r') . (`DivMm` l')
          ('/', SubMm l' r') -> (`SubMm` r') . (`DivMm` l')
          ('/', _) -> (`DivMm` r)
          _ -> error "macroModifier - can't be here"
    negate_ = (char '-' >> pure NegMm) <|> pure id
    const_ = ConstantMm <$> try double
    var_ = VarMm <$> (char '$' >> decimalInt)
    leaf_ = const_ <|> var_
    paren_ = ParenMm <$> (char '(' *> start_ <* space <* char ')')
    legacyAllowCapitalX = 'x' <$ char 'X'

macroPrimitive :: Parser MacroPrimitive
macroPrimitive =
  choice
    [ comment_
    , circle_
    , vectorLine_
    , centerLine_
    , outline_
    , polygon_
    , moire_
    , thermal_
    ] <?>
  "MacroPrimitive"
  where
    comment_
    -- according to spec command should end with * but according to examples
    -- in spec some comments are ended by newline too.
     =
      (char '0' >> (void (char ' ') <|> pure ())) *>
      (CommentMp <$> stringContent) <*
      (blockEnd <|> void eol)
    maybe_ p = fmap Just (try p) <|> pure Nothing
    comma_ p = void (char ',') >> p
    exposure_ =
      (ExposureOn <$ char '1') <|> (ExposureOff <$ char '0') <|>
      (ExposureVar <$> (char '$' >> decimalInt))
    circle_ = do
      void $ char '1'
      (CircleMp <$> comma_ exposure_ <*> comma_ macroModifier <*> comma_ macroModifier <*>
       comma_ macroModifier <*>
       maybe_ (comma_ macroModifier)) <*
        blockEnd <?> "CircleMp"
    vectorLine_ = do
      void $ string "20"
      (VectorLineMp <$> comma_ exposure_ <*> comma_ macroModifier <*> comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       maybe_ (comma_ macroModifier)) <*
        blockEnd <?> "VectorLineMp"
    centerLine_ = do
      void $ string "21"
      (CenterLineMp <$> comma_ exposure_ <*> comma_ macroModifier <*> comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       maybe_ (comma_ macroModifier)) <*
        blockEnd <?> "CenterLineMp"
    outline_ = do
      void $ char '4'
      e <- comma_ exposure_
      -- n is a modifier and can technically according to the spec be any expression but
      -- I am pretty sure that is an error by ommision in the spec.
      n <- (1 +) <$> comma_ decimalInt
      when (n < 2) (fail "number of subsequent points must be greater than 0")
      (OutlineMp e <$>
       count n ((,) <$> comma_ macroModifier <*> comma_ macroModifier) <*>
       maybe_ (comma_ macroModifier)) <*
        blockEnd <?> "OutlineMp"
    polygon_ = do
      void $ char '5'
      (PolygonMp <$> comma_ exposure_ <*> comma_ decimalInt <*> comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       maybe_ (comma_ macroModifier)) <*
        blockEnd <?> "PolygonMp"
    moire_ = do
      void $ char '6'
      (MoireMp <$> comma_ macroModifier <*> comma_ macroModifier <*> comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       maybe_ (comma_ macroModifier)) <*
        blockEnd <?> "MoireMp"
    thermal_ = do
      void $ char '7'
      (ThermalMp <$> comma_ macroModifier <*> comma_ macroModifier <*> comma_ macroModifier <*>
       comma_ macroModifier <*>
       comma_ macroModifier <*>
       maybe_ (comma_ macroModifier)) <*
        blockEnd <?> "ThermalMp"

macroVariable :: Parser MacroVariable
macroVariable =
  try $
  label "macroVariable" $
  char '$' *> (MacroVariable <$> (decimalInt <* char '=') <*> macroModifier) <*
  blockEnd

macroContent :: Parser MacroContent
macroContent =
  (VariableMc <$> macroVariable) <|> (PrimitiveMc <$> macroPrimitive)

macroAt :: Parser MacroAt
macroAt =
  extendStart $
  do void $ string "AM"
     (MacroAt <$> (name <* blockEnd) <*>
      manyTill macroContent (lookAhead extendEnd)) <*
       extendEnd

objectOptions :: Parser ObjectOptions
objectOptions =
  choice
    [ LoadPolarity <$>
      (PolarityDark <$ extendedString "LPD" <|> PolarityClear <$ extendedString "LPC")
    , LoadMirroring <$>
      choice
        [ DontMirror <$ extendedString "LMN"
        , MirrorX <$ extendedString "LMX"
        , MirrorY <$ extendedString "LMY"
        , MirrorXY <$ extendedString "LMXY"
        ]
    , LoadRotation <$> extended (string "LR" *> double)
    , LoadScaling <$> extended (string "LS" *> double)
    ] <*
  skipMany eol <?> "ObjectOptions"

attribute :: Parser Attribute
attribute = try $ extendStart (Attribute <$> tp <*> name <*> manyTill fld blockEnd)
  where
    tp =
      choice
        [ FileAttribute <$ string "TF"
        , AperatureAttribute <$ string "TA"
        , ObjectAttribute <$ string "TO"
        ]
    fld = char ',' *> field

attributeCommand :: Parser AttributeCommand
attributeCommand = try $ label "AttributeCommand" $
  (DefineAc <$> attribute) <|>
  extendStart
    (string "TD" *> (DeleteAc <$> (fmap Just name <|> pure Nothing)) <* blockEnd)

deprecatedCommand :: Parser DeprecatedCommand
deprecatedCommand = try $
  choice
    [ SelectAperatureDc <$ string "G54" <* blockEnd
    , PrepareFlashDc <$ string "G55" <* blockEnd
    , SetUnitInchDc <$ string "G70" <* blockEnd
    , SetUnitMmDc <$ string "G71" <* blockEnd
    , CoordinateFormatAbsoluteDc <$ string "G90" <* blockEnd
    , CoordinateFormatIncrementalDc <$ string "G91" <* blockEnd
    , extendStart (ProgrammingStopDc <$ string "M00" <* extendEnd)
    , extendStart (OptionalStopDc <$ string "M01" <* extendEnd)
    , extendStart
        (ImagePolarityDc <$ string "IP" <*> manyTill anyChar extendEnd)
    , extendStart
        (AxesCorrespondanceDc <$ string "AS" <*> manyTill anyChar extendEnd)
    , extendStart
        (ImageRotationDc <$ string "IR" <*> manyTill anyChar extendEnd)
    , extendStart (ImageMirrorDc <$ string "MI" <*> manyTill anyChar extendEnd)
    , extendStart (ImageOffsetDc <$ string "OF" <*> manyTill anyChar extendEnd)
    , extendStart (ScaleFactorDc <$ string "SF" <*> manyTill anyChar extendEnd)
    , extendStart (ImageNameDc <$ string "IN" <*> manyTill anyChar extendEnd)
    , extendStart (CommentDc <$ string "LN" <*> manyTill anyChar extendEnd)
    ]

blockCommand :: Parser BlockCommand
blockCommand =
  choice
    [ GrapicsStateBc <$> (graphicsState <|> graphicsStateDeprecatedLegacy)
    , InterpolateToBc <$> interpolateTo
    , MoveToBc <$> moveTo
    , FlashToBc <$> flashTo
    , RegionBc <$> region
    , SetAperatureBc <$> setAperature
    , ObjectOptionsBc <$> objectOptions
    , BlockAperatureBc <$> blockAperature
    , StepRepeatBc <$> stepRepeat
    , AttributeBc <$> attributeCommand
    , DeprecatedBc <$> deprecatedCommand
    , CommentBc <$> comment
    ] <?>
  "BlockCommand"

blockAperature :: Parser BlockAperature
blockAperature = try $ label "BlockAperature" $
  BlockAperature <$> extendStart (string "ABD" *> decimalInt <* extendEnd) <*>
  manyTill blockCommand (extendedString "AB")

stepRepeat :: Parser StepRepeat
stepRepeat = try $ label "StepRepeat" $ startSR <*> manyTill blockCommand endSR
  where
    endSR = void (extendedString "SR") <|> void (lookAhead startSR)
    startSR =
      extended
        (string "SR" *>
         (StepRepeat <$> (char 'X' *> decimalInt) <*> (char 'Y' *> decimalInt) <*>
          (char 'I' *> double) <*>
          (char 'J' *> double)))

gerberCommand :: Parser GerberCommand
gerberCommand =
  choice
    [ AperatureDefGb <$> aperatureDef
    , MacroGb <$> macroAt
    , CoordinateFormatGb <$> coordinateFormat
    , UnitsGb <$> units
    , BlockCommandGb <$> blockCommand
    ] <?> "GerberCommand"

gerberFile :: Parser GerberFile
gerberFile = many blockEnd *> go id
  where
    go cs = do
      c <- gerberCommand
      let cs' = cs . (c :)
      case c of
        CoordinateFormatGb fmt ->
          cs' <$>
          local
            (set lActiveCoordinateFormat (Just fmt))
            (manyTill gerberCommand endOfFileCode)
        _ -> go cs'
