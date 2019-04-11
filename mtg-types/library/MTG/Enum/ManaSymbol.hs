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
import qualified "parsers" Text.Parser.Tokens      as P

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

newtype ManaSymbol = ManaSymbol Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseManaSymbol'@

instance IsString ManaSymbol where

  fromString = (coerce . fromString)

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

noManaSymbol :: ManaSymbol
noManaSymbol = ""

--------------------------------------------------

whiteSymbol :: ManaSymbol
whiteSymbol = "W"

blueSymbol :: ManaSymbol
blueSymbol = "U"

blackSymbol :: ManaSymbol
blackSymbol = "B"

redSymbol :: ManaSymbol
redSymbol = "R"

greenSymbol :: ManaSymbol
greenSymbol = "G"

--------------------------------------------------

colorlessSymbol :: ManaSymbol
colorlessSymbol = "C"

snowSymbol :: ManaSymbol
snowSymbol = "S"

energySymbol :: ManaSymbol
energySymbol = "E"

variableSymbol :: ManaSymbol
variableSymbol = "X"

--------------------------------------------------

phyrexian :: ManaSymbol -> ManaSymbol
phyrexian (ManaSymbol s) = ManaSymbol ("{P" <> s <> "}")

phyrexianWhite :: ManaSymbol
phyrexianWhite = phyrexian whiteSymbol

phyrexianBlue :: ManaSymbol
phyrexianBlue = phyrexian blueSymbol

phyrexianBlack :: ManaSymbol
phyrexianBlack = phyrexian blackSymbol

phyrexianRed :: ManaSymbol
phyrexianRed = phyrexian redSymbol

phyrexianGreen :: ManaSymbol
phyrexianGreen = phyrexian greenSymbol

--------------------------------------------------

monohybrid :: ManaSymbol -> ManaSymbol
monohybrid (ManaSymbol s) = ManaSymbol ("{2/" <> s <> "}")

monohybridWhite :: ManaSymbol
monohybridWhite = monohybrid whiteSymbol

monohybridBlue :: ManaSymbol
monohybridBlue = monohybrid blueSymbol

monohybridBlack :: ManaSymbol
monohybridBlack = monohybrid blackSymbol

monohybridRed :: ManaSymbol
monohybridRed = monohybrid redSymbol

monohybridGreen :: ManaSymbol
monohybridGreen = monohybrid greenSymbol

--------------------------------------------------

genericSymbol :: Natural -> ManaSymbol
genericSymbol n = ManaSymbol $ (fromString . show) n

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

toManaSymbol :: Maybe Text -> ManaSymbol
toManaSymbol = maybe noManaSymbol ManaSymbol

--------------------------------------------------

colorToManaSymbol :: Color -> ManaSymbol
colorToManaSymbol = _

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppManaSymbol'@

instance Pretty ManaSymbol where

  pretty = ppManaSymbol

--------------------------------------------------

-- | @≡ 'ppManaSymbol'@

prettyManaSymbol :: ManaSymbol -> String
prettyManaSymbol (ManaSymbol cs) = PP.String.renderString ppManaSymbol

--------------------------------------------------

-- | 

ppManaSymbol :: ManaSymbol -> PP.Doc i
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

-- | @≡ 'pManaSymbol'@

parseManaSymbol :: (MonadThrow m) => String -> m ManaSymbol
parseManaSymbol = runParser pManaSymbol 

--------------------------------------------------

pManaSymbol :: (MTGParsing m) => m ManaSymbol
pManaSymbol = P.braces (pAbbreviatedManaSymbol `P.sepBy1` P.spaces)
 
--------------------------------------------------

pAbbreviatedManaSymbol :: (MTGParsing m) => m ManaSymbol
pAbbreviatedManaSymbol = _

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