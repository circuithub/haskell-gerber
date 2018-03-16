{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Data.Gerber.Types
--
-- AST representing the gerber file format as
-- specified in https://www.ucamco.com/files/downloads/file/81/the_gerber_file_format_specification.pdf .
--
-- Gerber is a command stream format but the AST is hierachical. The reason is that the
-- AST attempts to make it impossible to represent an invalid gerber file. That is certain
-- command place the interpreter in a certain context and in certain contexts certain commands
-- are not valid.
--
-- The gerber file is encoded as the 7-bit ASCII codes 32 to 126 and 10 and 13, or
-- printable ASCII and newlines. Newlines have no affect and can be ignored when
-- processing a file.
--
-- Gerber file is build up out of data blocks with each block ending with a '*'.
-- Extended commands starting with '%' and ending with '*%' may contain multiple data blocks.
--

module Data.Gerber.Types where

import Prelude

import Data.Text (Text)
import Data.Monoid
import Data.Functor.Foldable
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import Data.List (elem)
import Data.Maybe

-- | A gerber file is a sequence of top level gerber commands
type GerberFile = [GerberCommand]

-- | names in gerber are case sensitive and [a-zA-Z_.$]{[a-zA-Z_.0- 9]+}
newtype Name = Name
  { getName :: Text
  } deriving (Show, Ord, Eq)

-- | Alpha character in ascii
isAlpha_ascii :: Char -> Bool
isAlpha_ascii c = isAsciiLower c || isAsciiUpper c


{-# ANN isNameFirst ("HLint: ignore Redundant bracket" :: String) #-}
-- | Is the character a valid first character in a "Name"
isNameFirst :: Char -> Bool
isNameFirst = getAny . (Any . isAlpha_ascii <> Any . (`elem` ("_.$" :: String)))

{-# ANN isNameNotFirst ("HLint: ignore Redundant bracket" :: String) #-}
-- | Is the character a valid non first character in a "Name"
isNameNotFirst :: Char -> Bool
isNameNotFirst = getAny . (Any . isDigit <> Any . isNameFirst)

-- | Fields are the same as "StringContent" except commas are not allowed
newtype Field = Field
  { getField :: Text
  } deriving (Show, Eq, Ord)

{-# ANN isField ("HLint: ignore Redundant bracket" :: String) #-}
-- | Is the character a valid "Field" character
isField :: Char -> Bool
isField =
  getAny . (Any . isNameNotFirst <> Any . (`elem` ("-+/!?<>”’(){}\\|&@# ;:=" :: String)))

-- | Strings in geber are case sensitive and [a-zA-Z0-9_+-/!?<>”’(){}. \|&@#,;$:=]+
newtype StringContent = StringContent
  { getStringContent :: Text
  } deriving (Show, Eq, Ord)

{-# ANN isStringContent ("HLint: ignore Redundant bracket" :: String) #-}
-- | Is the character a valid "String" character
isStringContent :: Char -> Bool
isStringContent = getAny . (Any . isField <> Any . (== ','))

-- | Coordinates can optionally be part of "InterpolateTo", "MoveTo"
--   and "FlashTo" operations (codes D01, D02 or D03) and if they
--   are missing the previously set coordinates are used.
data CoordinateModify = CoordinateModify
  { coordinateModifyX :: Maybe Double
  , coordinateModifyY :: Maybe Double
  } deriving (Show, Eq, Ord)

-- | Circular offset used with "InterpolateTo" operation
data CircularOffset = CircularOffset
  { circularOfsetX :: Double
  , circularOfsetY :: Double
  } deriving (Show, Eq, Ord)

-- | Command D01.
--   Depending on interpolation mode either draw a line or arc from current position
--   to newly updated position using. Outside of region mode it strokes using the
--   current aperature, which must be a standard circle or rectangle. Inside region
--   mode it defines the contour of a region polygon, ignoring the aperature.
data InterpolateTo = InterpolateTo
      { getInterpolateTo :: CoordinateModify
      , getCircularOffset :: Maybe CircularOffset}
  deriving (Show, Eq, Ord)

-- | Command D02.
--   Change the position to the newly updated position
newtype MoveTo = MoveTo { getMoveTo :: CoordinateModify} deriving (Show, Eq, Ord)

-- | Command D03.
--   Flash the current aperature at the newly modified position
newtype FlashTo = FlashTo { getFlashTo :: CoordinateModify} deriving (Show, Eq, Ord)

-- | Set the current aperature to the previously defined aperature.
newtype SetAperature = SetAperature
  { setAperatureId :: AperaturId
  } deriving (Show, Eq, Ord)

-- | Sets the interpolation state for the graphics
data GraphicsState
  = SetInterpolationLinear
  | SetInterpolationCircularClockWise
  | SetInterpolationCircularCounterClockWise
  | SetInterpolationCircularSingleQuadrant
  | SetInterpolationCircularMultiQuadrant
  deriving (Show, Eq, Ord)

-- | The commands allowed inside region mode
data RegionCommand
  = InterpolateToRc InterpolateTo
  | MoveToRc MoveTo
  | StateRc GraphicsState
  | CommentRc Comment
  deriving (Show, Eq, Ord)

-- | Region mode starting with G36 and ending with G37
newtype RegionF a = Region
  { getRegionCommands :: [a]
  } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

type Region = RegionF RegionCommand

-- | Comment is a G04 followed by "StringConent"
newtype Comment = Comment
  { getComment :: StringContent
  } deriving (Show, Eq, Ord)


-- | End of the gerber file MO2
data EndOfFileCode = EndOfFileCode deriving (Show, Eq, Ord)

-- | The information about how to decode "CoordinateModify" and "CircularOffset"
--   Number decimal character sequence gets extended to the left by '0' till
--   lenght equals "coordinateFormatIntegers" + "coordinateFormatDecimals".  The
--   decimal point is then placed after "coordinateFormatIntegers" number of characters.
data CoordinateFormat = CoordinateFormat
  { coordinateFormatIntegers :: Int
  , coordinateFormatDecimals :: Int
  } deriving (Show, Eq, Ord)

-- | Units of all values in the gerber file, must appear in the header before any command
--   that requires units.
data Units
  = Inches
  | Milimeters
  deriving (Show, Eq, Ord)

-- | Identifies an aperature definition.
newtype AperaturId = AperaturId
  { getAperaturId :: Int
  } deriving (Show, Eq, Ord)

-- | Aperature definitions using one of the standard aperature templates
data StrokeAd
  = CircleStrokeAd Double
  | RectangleStrokeAd { rectangleStrokeAdX :: Double
                      , rectangleStrokeAdY :: Double}
  deriving (Show, Eq, Ord)

-- | Aperture definitions using one of the standard aperature templates
data StandardAd
  = CircleStandardAd { circleStandardAdDiameter :: Double, circleStandardAdHoleDiameter :: Double}
  | RectangleStandardAd { rectangleStandardAdX :: Double
                 , rectangleStandardAdY :: Double
                 , rectangleStandardAdHoleDiameter :: Double}
  | ObroundStandardAd { obroundStandardAdX :: Double
               , obroundStandardAdY :: Double
               , obroundStandardAdHoleDiameter :: Maybe Double}
  | PolygonStandardAd { polygonStandardAdDiamter :: Double
               , polygonStandardAdVertices :: Int
               , polygonStandardAdRotation :: Maybe Double
               , polygonStandardAdHoleDiameter :: Maybe Double}
  deriving (Show, Eq, Ord)

-- | Aperature definition making use of a previously define macro aperature template.
data MacroAd = MacroAd
  { macroAdTempalateId :: Name   -- ^ Name of the macro template to instantiate
  , macroAdModifiers :: [Double] -- ^ Values used for variable substitution
  }
  deriving (Show, Eq, Ord)

-- | The type of aperature being defined
data AdType
  = StrokeAdt StrokeAd
  | StandardAdt StandardAd
  | MacroAdt MacroAd
  deriving (Show, Eq, Ord)

-- | Define a macro apperture binding it to the specified ID. It cannot be rebound
data AperatureDef = AperatureDef
  { aperatureDefId :: AperaturId
  , aperatureDefType :: AdType
  } deriving (Show, Eq, Ord)

-- | In a macro template define the exposure of a primitive
-- Theoretically according to the spec
-- all modifiers can be expressions but
-- I can't imagine any argument other than
-- variable substitution being valid for exposure
data Exposure
  = ExposureOn  -- ^ 1
  | ExposureOff -- ^ 0
  | ExposureVar Int -- ^ The ID of the variable that should evaluate to 1 or 0
  deriving (Show, Eq, Ord)

-- | Macro modifiers used in macro templates from a little expression language
--   on doubles following normal mathematical precedance on operations
data MacroModifierF a
  = ConstantMmF Double -- ^ A consant
  | VarMmF Int -- ^ Variable lookup
  | NegMmF a -- ^ Negation
  | AddMmF a a -- ^ Addition
  | SubMmF a a -- ^ Subtraction
  | MulMmF a a -- ^ Mutliplication
  | DivMmF a a -- ^ Division
  | ParenMmF a -- ^ Parentheses around an expression
  deriving (Show, Eq, Ord, Functor)

newtype MacroModifier = MacroModifier {macroModifierF :: MacroModifierF MacroModifier} deriving (Show, Eq, Ord)
-- |
type instance Base MacroModifier = MacroModifierF
-- |
instance Recursive MacroModifier where project = macroModifierF
-- |
instance Corecursive MacroModifier where embed = MacroModifier

pattern ConstantMm :: Double -> MacroModifier
pattern ConstantMm a = MacroModifier (ConstantMmF a)
pattern VarMm :: Int -> MacroModifier
pattern VarMm a = MacroModifier (VarMmF a)
pattern NegMm :: MacroModifier -> MacroModifier
pattern NegMm a = MacroModifier (NegMmF a)
pattern AddMm :: MacroModifier -> MacroModifier -> MacroModifier
pattern AddMm a b = MacroModifier (AddMmF a b)
pattern SubMm :: MacroModifier -> MacroModifier -> MacroModifier
pattern SubMm a b = MacroModifier (SubMmF a b)
pattern MulMm :: MacroModifier -> MacroModifier -> MacroModifier
pattern MulMm a b = MacroModifier (MulMmF a b)
pattern DivMm :: MacroModifier -> MacroModifier -> MacroModifier
pattern DivMm a b = MacroModifier (DivMmF a b)
pattern ParenMm :: MacroModifier -> MacroModifier
pattern ParenMm a = MacroModifier (ParenMmF a)

-- | The parametrized primitives that may be drawn as part of a macro template
data MacroPrimitive
  = CommentMp StringContent
  | CircleMp { circleMpExposure :: Exposure
             , circleMpDiameter :: MacroModifier
             , circleMpX :: MacroModifier
             , circleMpY :: MacroModifier
             , circleMpRotation :: Maybe MacroModifier}
  | VectorLineMp { vectorLineMpExposure :: Exposure
                 , vectorLineMpWidth :: MacroModifier
                 , vectorLineMpStartX :: MacroModifier
                 , vectorLineMpStartY :: MacroModifier
                 , vectorLineMpEndX :: MacroModifier
                 , vectorLineMpEndY :: MacroModifier
                 , vectorLineMpRotation :: Maybe MacroModifier}
  | CenterLineMp { centerLineMpExposure :: Exposure
                 , centerLineMpWidth :: MacroModifier
                 , centerLineMpHeight :: MacroModifier
                 , centerLineMpX :: MacroModifier
                 , centerLineMpY :: MacroModifier
                 , centerLineMpRotation :: Maybe MacroModifier}
  | OutlineMp { outlineMpExposure :: Exposure
              , outlineMpPoints :: [(MacroModifier, MacroModifier)]
              , outlineMpRotation :: Maybe MacroModifier}
  | PolygonMp { polygonMpExposure :: Exposure
              , polygonMpVertices :: Int -- technically according to the spec any expression but assuming that is an error by ommision
              , polygonMpX :: MacroModifier
              , polygonMpY :: MacroModifier
              , polygonMpDiameter :: MacroModifier
              , polygonMpRotation :: Maybe MacroModifier}
  | MoireMp { moireMpX :: MacroModifier
            , moireMpY :: MacroModifier
            , moireMpDiameter :: MacroModifier
            , moireMpRingThickness :: MacroModifier
            , moireMpRingGap :: MacroModifier
            , moireMpMaxRingCount :: MacroModifier
            , moireMpCrossHairThickness :: MacroModifier
            , moireMpCrossHairLength :: MacroModifier
            , moireMpRotation :: Maybe MacroModifier}
  | ThermalMp { thermalMpX :: MacroModifier
              , thermalMpY :: MacroModifier
              , thermalMpOuterDiameter :: MacroModifier
              , thermalMpInnerDiameter :: MacroModifier
              , thermalMpGapThickness :: MacroModifier
              , thermalMpRotation :: Maybe MacroModifier}
  deriving (Show, Eq, Ord)

-- | Defines a macro variable based on a macro modifier expression overriding previous variables by same name
data MacroVariable = MacroVariable
  { macroVariableName :: Int
  , macroVariableExpression :: MacroModifier
  } deriving (Show, Eq, Ord)

-- | A macro template consists of a series of primitives or variable definitions
data MacroContent
  = PrimitiveMc MacroPrimitive
  | VariableMc MacroVariable
  deriving (Show, Eq, Ord)

-- | A macro template with its name and content
data MacroAt = MacroAt
  { macroAtTemplateName :: Name
  , macroAtConent :: [MacroContent]
  } deriving (Show, Eq, Ord)

-- | Set the polarity used to draw
data Polarity
  = PolarityClear
  | PolarityDark
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Whether the thing to draw should be mirrored
data Mirroring
  = DontMirror
  | MirrorX
  | MirrorY
  | MirrorXY
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Options affecting how objects are drawn
data ObjectOptions
  = LoadPolarity Polarity
  | LoadMirroring Mirroring
  | LoadRotation Double
  | LoadScaling Double
  deriving (Show, Eq, Ord)

data AttributeType
  = FileAttribute
  | AperatureAttribute
  | ObjectAttribute
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Additional attributes that conveys non drawing information
data Attribute = Attribute
  { attributeType :: AttributeType
  , attributeName :: Name
  , attributeFields :: [Field]
  } deriving (Show, Eq, Ord)

data AttributeCommand
  = DefineAc Attribute
  | DeleteAc (Maybe Name)
  deriving (Show, Eq, Ord)

-- | Set of deprecated commands mostly included here so they can be parsed out
data DeprecatedCommand
  = SelectAperatureDc
  | PrepareFlashDc
  | SetUnitInchDc
  | SetUnitMmDc
  | CoordinateFormatAbsoluteDc
  | CoordinateFormatIncrementalDc
  | ProgrammingStopDc
  | OptionalStopDc
  | ImagePolarityDc String
  | AxesCorrespondanceDc String
  | ImageRotationDc String
  | ImageMirrorDc String
  | ImageOffsetDc String
  | ScaleFactorDc String
  | ImageNameDc String
  | CommentDc String
  deriving (Show, Eq, Ord)

-- | Commands usable inside a block aperature or step repeat operations
data BlockCommand
  = GrapicsStateBc GraphicsState
  | InterpolateToBc InterpolateTo
  | MoveToBc MoveTo
  | FlashToBc FlashTo
  | RegionBc Region
  | SetAperatureBc SetAperature
  | ObjectOptionsBc ObjectOptions
  | BlockAperatureBc BlockAperature
  | StepRepeatBc StepRepeat
  | AttributeBc AttributeCommand
  | DeprecatedBc DeprecatedCommand
  | CommentBc Comment
  deriving (Show, Eq, Ord)

-- | Reusable set of drawing commands
data BlockAperatureF a = BlockAperature
  { blockAperatureId :: Int
  , blockAperatureCommands :: [a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type BlockAperature = BlockAperatureF BlockCommand

-- | Step and repeat a set of drawing commands
data StepRepeatF a = StepRepeat
  { stepRepeatRepeatX :: Int
  , stepRepeatRepeatY :: Int
  , stepRepeatStepX :: Double
  , stepRepeatStepY :: Double
  , stepRepeatCommands :: [a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type StepRepeat = StepRepeatF BlockCommand

-- | Top level of commands in a gerber file
data GerberCommand
  = AperatureDefGb AperatureDef
  | MacroGb MacroAt
  | CoordinateFormatGb CoordinateFormat
  | UnitsGb Units
  | BlockCommandGb BlockCommand
  deriving (Show, Eq, Ord)

-- | A gerber file represented as a recursive Functor
--   to aid uniform traversal
data GerberCommandF a
  = GerberFileF [a]
  | AperatureDefF AperatureDef
  | MacroF MacroAt
  | CoordinateFormatF CoordinateFormat
  | UnitsF Units
  | BlockCommandF a
  | GrapicsStateF GraphicsState
  | InterpolateToF InterpolateTo
  | MoveToF MoveTo
  | FlashToF FlashTo
  | RegionF (RegionF a)
  | SetAperatureF SetAperature
  | ObjectOptionsF ObjectOptions
  | BlockAperatureF (BlockAperatureF a)
  | StepRepeatF (StepRepeatF a)
  | AttributeF AttributeCommand
  | DeprecatedF DeprecatedCommand
  | CommentF Comment
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | A more convenient fixed point of GerberCommandF that can be shown etc
newtype FixGerberCommandF = FixGerberCommandF {unfixGerberCommandF :: GerberCommandF FixGerberCommandF} deriving (Show, Eq, Ord)
-- | Make FixGerberCommandF usable with recursion-schemes
type instance Base FixGerberCommandF = GerberCommandF
-- | Make FixGerberCommandF usable with recursion-schemes
instance Recursive FixGerberCommandF where project = unfixGerberCommandF
-- | Make FixGerberCommandF usable with recursion-schemes
instance Corecursive FixGerberCommandF where embed = FixGerberCommandF

-- | Convert a GerberFile to the equivelant representation as the fixed point of the GerberCommandF functor
toGerberFileF :: GerberFile -> Fix GerberCommandF
toGerberFileF = Fix . GerberFileF . map toGerberCommandF

-- | Helper used by "toGerberFileF" to convert a single gerber command
toGerberCommandF :: GerberCommand -> Fix GerberCommandF
toGerberCommandF c =
  Fix $
  case c of
    AperatureDefGb a -> AperatureDefF a
    MacroGb a -> MacroF a
    CoordinateFormatGb a -> CoordinateFormatF a
    UnitsGb a -> UnitsF a
    BlockCommandGb a -> BlockCommandF (fromBlockCommand a)
  where
    fromBlockCommand :: BlockCommand -> Fix GerberCommandF
    fromBlockCommand c' =
      Fix $
      case c' of
        GrapicsStateBc a -> GrapicsStateF a
        InterpolateToBc a -> InterpolateToF a
        MoveToBc a -> MoveToF a
        FlashToBc a -> FlashToF a
        RegionBc a -> RegionF (fromRegionCommand <$> a)
        SetAperatureBc a -> SetAperatureF a
        ObjectOptionsBc a -> ObjectOptionsF a
        BlockAperatureBc a -> BlockAperatureF (fromBlockCommand <$> a)
        StepRepeatBc a -> StepRepeatF (fromBlockCommand <$> a)
        AttributeBc a -> AttributeF a
        DeprecatedBc a -> DeprecatedF a
        CommentBc a -> CommentF a
    ------
    fromRegionCommand :: RegionCommand -> Fix GerberCommandF
    fromRegionCommand c' =
      Fix $
      case c' of
        InterpolateToRc a -> InterpolateToF  a
        MoveToRc a -> MoveToF a
        StateRc a -> GrapicsStateF a
        CommentRc a -> CommentF a

-- | Convert from the fixed point functor representation to the concrete gerber file format.
--   May fail if the functor does not represent a valid hierarchy.
fromGerberFileF :: Fix GerberCommandF -> Maybe GerberFile
fromGerberFileF f =
  case unfix f of
    GerberFileF cs -> traverse fromGerberCommandF cs
    _ -> Nothing

-- | Helper used by "fromGerberFileF" to convert a single gerber command
fromGerberCommandF :: Fix GerberCommandF -> Maybe GerberCommand
fromGerberCommandF = cata f
  where
    f :: GerberCommandF (Maybe GerberCommand) -> Maybe GerberCommand
    f (GerberFileF _) = Nothing
    f (AperatureDefF a) = Just (AperatureDefGb a)
    f (MacroF a) = Just (MacroGb a)
    f (CoordinateFormatF a) = Just (CoordinateFormatGb a)
    f (UnitsF a) = Just (UnitsGb a)
    f (BlockCommandF a) = BlockCommandGb <$> onlyBlockCommand a
    f (GrapicsStateF a) = Just . BlockCommandGb $ GrapicsStateBc a
    f (InterpolateToF a) = Just . BlockCommandGb $ InterpolateToBc a
    f (MoveToF a) = Just . BlockCommandGb $ MoveToBc a
    f (FlashToF a) = Just . BlockCommandGb $ FlashToBc a
    f (RegionF a) = BlockCommandGb . RegionBc <$> traverse onlyRegionCommand a
    f (SetAperatureF a) = Just . BlockCommandGb $ SetAperatureBc a
    f (ObjectOptionsF a) = Just . BlockCommandGb $ ObjectOptionsBc a
    f (BlockAperatureF a) =
      BlockCommandGb . BlockAperatureBc <$> traverse onlyBlockCommand a
    f (StepRepeatF a) =
      BlockCommandGb . StepRepeatBc <$> traverse onlyBlockCommand a
    f (AttributeF a) = Just . BlockCommandGb $ AttributeBc a
    f (DeprecatedF a) = Just . BlockCommandGb $ DeprecatedBc a
    f (CommentF a) = Just . BlockCommandGb $ CommentBc a
    ----------
    onlyBlockCommand (Just (BlockCommandGb a)) = Just a
    onlyBlockCommand _ = Nothing
    -----------
    onlyRegionCommand (Just (BlockCommandGb (InterpolateToBc a))) =
      Just (InterpolateToRc a)
    onlyRegionCommand (Just (BlockCommandGb (MoveToBc a))) =
      Just (MoveToRc a)
    onlyRegionCommand (Just (BlockCommandGb (GrapicsStateBc a))) =
      Just (StateRc a)
    onlyRegionCommand (Just (BlockCommandGb (CommentBc a))) = Just (CommentRc a)
    onlyRegionCommand _ = Nothing

-- | Utility function to collect a monoid over a gerber file wth a bottom up traversal by
-- supplying a function to optionally override the default action at each node.
-- The default actions is `mempty` for leaves and `mconcat` for intermediate nodes.
collectGerberFileF
  :: forall m.
     Monoid m
  => (GerberCommandF m -> Maybe m) -> Fix GerberCommandF -> m
collectGerberFileF f = cata alg
  where
    alg :: GerberCommandF m -> m
    alg n@(GerberFileF as) = fromMaybe (mconcat as) (f n)
    alg n@(AperatureDefF _) = fromMaybe mempty (f n)
    alg n@(MacroF _) = fromMaybe mempty (f n)
    alg n@(CoordinateFormatF _) = fromMaybe mempty (f n)
    alg n@(UnitsF _) = fromMaybe mempty (f n)
    alg n@(BlockCommandF a) = fromMaybe a (f n)
    alg n@(GrapicsStateF _) = fromMaybe mempty (f n)
    alg n@(InterpolateToF _) = fromMaybe mempty (f n)
    alg n@(MoveToF _) = fromMaybe mempty (f n)
    alg n@(FlashToF _) = fromMaybe mempty (f n)
    alg n@(RegionF _) = fromMaybe mempty (f n)
    alg n@(SetAperatureF _) = fromMaybe mempty (f n)
    alg n@(ObjectOptionsF _) = fromMaybe mempty (f n)
    alg n@(BlockAperatureF a) =
      fromMaybe (mconcat (blockAperatureCommands a)) (f n)
    alg n@(StepRepeatF a) = fromMaybe (mconcat (stepRepeatCommands a)) (f n)
    alg n@(AttributeF _) = fromMaybe mempty (f n)
    alg n@(DeprecatedF _) = fromMaybe mempty (f n)
    alg n@(CommentF _) = fromMaybe mempty (f n)
