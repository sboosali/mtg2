--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

--------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-| 

== Examples

Printing...

>>> pretty (ManaSymbol "U")
{U}
>>> pretty (colorToManaSymbol Blue)
{U}

Parsing...

>>> parseManaSymbol "{U}"
ManaSymbol "U"

-}

module MTG.Text.ManaSymbol where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Text.Color

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP

--------------------------------------------------

-- import qualified "parsers" Text.Parser.Combinators as P
-- import qualified "parsers" Text.Parser.Char        as P
--import qualified "parsers" Text.Parser.Token       as P

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

newtype ManaSymbol = ManaSymbol

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseManaSymbol'@

instance IsString ManaSymbol where

  fromString = fromString_MonadThrow parseManaSymbol

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

pattern NoManaSymbol :: ManaSymbol
pattern NoManaSymbol = ""

--------------------------------------------------

pattern WhiteManaSymbol :: ManaSymbol
pattern WhiteManaSymbol = "W"

pattern BlueManaSymbol :: ManaSymbol
pattern BlueManaSymbol = "U"

pattern BlackManaSymbol :: ManaSymbol
pattern BlackManaSymbol = "B"

pattern RedManaSymbol :: ManaSymbol
pattern RedManaSymbol = "R"

pattern GreenManaSymbol :: ManaSymbol
pattern GreenManaSymbol = "G"

--------------------------------------------------

pattern ColorlessManaSymbol :: ManaSymbol
pattern ColorlessManaSymbol = "C"

pattern SnowManaSymbol :: ManaSymbol
pattern SnowManaSymbol = "S"

pattern EnergyManaSymbol :: ManaSymbol
pattern EnergyManaSymbol = "E"

--------------------------------------------------

pattern VariableXManaSymbol :: ManaSymbol
pattern VariableXManaSymbol = "X"

pattern VariableYManaSymbol :: ManaSymbol
pattern VariableYManaSymbol = "Y"

pattern VariableZManaSymbol :: ManaSymbol
pattern VariableZManaSymbol = "Z"

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

knownManaSymbols :: [ManaSymbol]
knownManaSymbols =

    [ WhiteManaSymbol
    , BlueManaSymbol
    , BlackManaSymbol
    , RedManaSymbol
    , GreenManaSymbol

    , ColorlessManaSymbol
    , SnowManaSymbol
    , EnergyManaSymbol

    , VariableXManaSymbol
    , VariableYManaSymbol
    , VariableZManaSymbol
    ]

--------------------------------------------------

namedManaSymbols :: Assoc ManaSymbol
namedManaSymbols =

    [ "white" -: WhiteManaSymbol
    , "blue"  -: BlueManaSymbol
    , "black" -: BlackManaSymbol
    , "red"   -: RedManaSymbol
    , "green" -: GreenManaSymbol


    , "colorless" -: ColorlessManaSymbol
    , "snow"      -: SnowManaSymbol
    , "energy"    -: EnergyManaSymbol

    , "variable"  -: VariableXManaSymbol
    ]

--------------------------------------------------

abbreviatedManaSymbols :: Assoc ManaSymbol
abbreviatedManaSymbols =

    [ "W" -: WhiteManaSymbol
    , "U" -: BlueManaSymbol
    , "B" -: BlackManaSymbol
    , "R" -: RedManaSymbol
    , "G" -: GreenManaSymbol

    , "C" -: ColorlessManaSymbol
    , "S" -: SnowManaSymbol
    , "E" -: EnergyManaSymbol

    , "X" -: ManaSymbol "X"
    , "Y" -: ManaSymbol "Y"
    , "Z" -: ManaSymbol "Z"
    ]

--------------------------------------------------

phyrexianWhite :: ManaSymbol
phyrexianWhite = phyrexian WhiteManaSymbol

phyrexianBlue :: ManaSymbol
phyrexianBlue = phyrexian BlueManaSymbol

phyrexianBlack :: ManaSymbol
phyrexianBlack = phyrexian BlackManaSymbol

phyrexianRed :: ManaSymbol
phyrexianRed = phyrexian RedManaSymbol

phyrexianGreen :: ManaSymbol
phyrexianGreen = phyrexian GreenManaSymbol

--------------------------------------------------

monohybridWhite :: ManaSymbol
monohybridWhite = monohybrid WhiteManaSymbol

monohybridBlue :: ManaSymbol
monohybridBlue = monohybrid BlueManaSymbol

monohybridBlack :: ManaSymbol
monohybridBlack = monohybrid BlackManaSymbol

monohybridRed :: ManaSymbol
monohybridRed = monohybrid RedManaSymbol

monohybridGreen :: ManaSymbol
monohybridGreen = monohybrid GreenManaSymbol

--------------------------------------------------

zeroSymbol :: ManaSymbol
zeroSymbol = genericSymbol 0

oneSymbol :: ManaSymbol
oneSymbol = genericSymbol 1

twoSymbol :: ManaSymbol
twoSymbol = genericSymbol 2

threeSymbol :: ManaSymbol
threeSymbol = genericSymbol 3

fourSymbol :: ManaSymbol
fourSymbol = genericSymbol 4

fiveSymbol :: ManaSymbol
fiveSymbol = genericSymbol 5

sixSymbol :: ManaSymbol
sixSymbol = genericSymbol 6

sevenSymbol :: ManaSymbol
sevenSymbol = genericSymbol 7

eightSymbol :: ManaSymbol
eightSymbol = genericSymbol 8

nineSymbol :: ManaSymbol
nineSymbol = genericSymbol 9

tenSymbol :: ManaSymbol
tenSymbol = genericSymbol 10

elevenSymbol :: ManaSymbol
elevenSymbol = genericSymbol 11

twelveSymbol :: ManaSymbol
twelveSymbol = genericSymbol 12

thirteenSymbol :: ManaSymbol
thirteenSymbol = genericSymbol 13

fourteenSymbol :: ManaSymbol
fourteenSymbol = genericSymbol 14

fifteenSymbol :: ManaSymbol
fifteenSymbol = genericSymbol 15

sixteenSymbol :: ManaSymbol
sixteenSymbol = genericSymbol 16

seventeenSymbol :: ManaSymbol
seventeenSymbol = genericSymbol 17

eighteenSymbol :: ManaSymbol
eighteenSymbol = genericSymbol 18

nineteenSymbol :: ManaSymbol
nineteenSymbol = genericSymbol 19

twentySymbol :: ManaSymbol
twentySymbol = genericSymbol 20

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Smart Constructor for 'ManaSymbol'.

toManaSymbol :: Text -> ManaSymbol
toManaSymbol = capitalize > ManaSymbol
  where

  capitalize = Text.toTitle

--------------------------------------------------

-- | Accessor for 'ManaSymbol'.

getManaSymbolText :: ManaSymbol -> Text
getManaSymbolText (ManaSymbol t) = t

--------------------------------------------------

colorToManaSymbol :: Color -> ManaSymbol
colorToManaSymbol c = go c
  where

  go
    = abbreviateColor
    > maybe (toManaSymbol (getColor c)) toManaSymbol

--------------------------------------------------

genericSymbol :: Natural -> ManaSymbol
genericSymbol n = ManaSymbol $ (fromString . show) n

--------------------------------------------------

phyrexian :: ManaSymbol -> ManaSymbol
phyrexian (ManaSymbol s) = ManaSymbol (s <> "/P")

--------------------------------------------------

monohybrid :: ManaSymbol -> ManaSymbol
monohybrid (ManaSymbol s) = ManaSymbol (s <> "/2")

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppManaSymbol'@

instance Pretty ManaSymbol where

  pretty = ppManaSymbol

--------------------------------------------------

-- | @≡ 'runPrinter' 'ppManaSymbol'@

prettyManaSymbol :: ManaSymbol -> String
prettyManaSymbol = runPrinter ppManaSymbol

--------------------------------------------------

-- | 

ppManaSymbol :: ManaSymbol -> Doc i
ppManaSymbol (ManaSymbol t) = PP.braces doc
  where

  doc = PP.pretty txt
  txt = t

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pManaSymbol'@

instance Parse ManaSymbol where

  parser = pManaSymbol

--------------------------------------------------

pManaSymbol :: (MTGParsing m) => m ManaSymbol
pManaSymbol = ManaSymbol <$> pSymbolText <?> "ManaSymbol"

--------------------------------------------------
{-

pAbbreviatedManaSymbol :: (MTGParsing m) => m ManaSymbol
pAbbreviatedManaSymbol = ManaSymbol <$> P.braces p
  where

  p = empty

-}
--------------------------------------------------

-- | @≡ 'pManaSymbol'@

parseManaSymbol :: (MonadThrow m) => String -> m ManaSymbol
parseManaSymbol = runParser 'ManaSymbol pManaSymbol

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''ManaSymbol

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{-$setup

>>> :set -XOverloadedStrings

-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-
--------------------------------------------------

ManaSymbol Char

--------------------------------------------------

e.g. « Mental Misstep »:

    {
                "name": "Mental Misstep",
                "manaCost": "{U/P}",
                "text": "({U/P} can be paid with either {U} or 2 life.)\nCounter target spell with converted mana cost 1.",
                "originalText": "({PU} can be paid with either {U} or 2 life.)\nCounter target spell with converted mana cost 1.",
                ...
    }

--------------------------------------------------



--------------------------------------------------
-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------