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

>>> pretty (colorToManaSymbol Blue)
{U}

-}

module MTG.Enum.ManaSymbol where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Enum.Color

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "parsers" Text.Parser.Combinators as P
import qualified "parsers" Text.Parser.Char        as P
import qualified "parsers" Text.Parser.Token       as P

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

newtype ManaSymbol = ManaSymbol Text
 
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

pattern VariableManaSymbol :: ManaSymbol
pattern VariableManaSymbol = "X"

--------------------------------------------------
-- Constants -------------------------------------
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

toManaSymbol :: Text -> ManaSymbol
toManaSymbol = ManaSymbol

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
phyrexian (ManaSymbol s) = ManaSymbol ("{P" <> s <> "}")

--------------------------------------------------

monohybrid :: ManaSymbol -> ManaSymbol
monohybrid (ManaSymbol s) = ManaSymbol ("{2/" <> s <> "}")

--------------------------------------------------

abbreviateManaSymbol :: ManaSymbol -> Maybe Text
abbreviateManaSymbol (ManaSymbol s0) = Text.toUpper <$> (go s1)
  where

  s1 = Text.toLower s0

  go = \case

    "white"     -> Just "W"
    "blue"      -> Just "U"
    "black"     -> Just "B"
    "red"       -> Just "R"
    "green"     -> Just "G"

    "w"         -> Just "W"
    "u"         -> Just "U"
    "b"         -> Just "B"
    "r"         -> Just "R"
    "g"         -> Just "G"

    _           -> Nothing

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppManaSymbol'@

instance Pretty ManaSymbol where

  pretty = ppManaSymbol

--------------------------------------------------

-- | @≡ 'ppManaSymbol'@

prettyManaSymbol :: ManaSymbol -> String
prettyManaSymbol = ppManaSymbol > runPrinter

--------------------------------------------------

-- | 

ppManaSymbol :: ManaSymbol -> Doc i
ppManaSymbol symbol = PP.braces docManaSymbol
  where

  docManaSymbol    = PP.pretty stringManaSymbol
  stringManaSymbol = (abbreviateManaSymbol symbol) & fromMaybe "?" -- TODO -- 

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pManaSymbol'@

instance Parse ManaSymbol where

  parser = pManaSymbol

--------------------------------------------------

pManaSymbol :: (MTGParsing m) => m ManaSymbol
pManaSymbol = do

  pAssoc cs <?> "ManaSymbol"

  where

  cs :: Assoc ManaSymbol
  cs = csLower <> csUpper

  csLower = csUpper <&> (bimap Text.toLower id)

  csUpper =

    [ "W" -: WhiteManaSymbol
    , "U" -: BlueManaSymbol
    , "B" -: BlackManaSymbol
    , "R" -: RedManaSymbol
    , "G" -: GreenManaSymbol
    ]

--------------------------------------------------

pAbbreviatedManaSymbol :: (MTGParsing m) => m ManaSymbol
pAbbreviatedManaSymbol = ManaSymbol <$> P.braces p
  where

  p = empty

--------------------------------------------------

-- | @≡ 'pManaSymbol'@

parseManaSymbol :: (MonadThrow m) => String -> m ManaSymbol
parseManaSymbol = runParser 'ManaSymbol pManaSymbol

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''ManaSymbol

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

 ManaSymbol Char

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------