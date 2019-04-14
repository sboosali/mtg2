--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------
--------------------------------------------------

{-| 

== Examples

Printing (see 'pretty'):

>>> pretty Blue
U
>>> Blue
Color "Blue"

Parsing (see 'parser'):

>>> parseColor "U"
Color "Blue"
>>> parseColor "blue"
Color "Blue"

-}

module MTG.Text.Color where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP

--------------------------------------------------

-- import qualified "parsers" Text.Parser.Combinators as P
-- import qualified "parsers" Text.Parser.Char        as P

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| A /Magic: The Gathering/ color.

-}

newtype Color = Color

  { getColor :: Text
  }

  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Show,Read)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Color where
  fromString = fromString_MonadThrow parseColor

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

-- | @≡ "White"@

pattern White :: Color
pattern White = "White"

-- | @≡ "Blue"@

pattern Blue :: Color
pattern Blue = "Blue"

-- | @≡ "Black"@

pattern Black :: Color
pattern Black = "Black"

-- | @≡ "Red"@

pattern Red :: Color
pattern Red = "Red"

-- | @≡ "Green"@

pattern Green :: Color
pattern Green = "Green"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

abbreviateColor :: Color -> Maybe Text
abbreviateColor (Color s0) = Text.toUpper <$> (go s1)
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

-- | @≡ 'PP.braces' . 'abbreviateColor'@

instance Pretty Color where

  pretty = ppColor

--------------------------------------------------

ppColor :: Color -> Doc i
ppColor color = PP.braces docColor
  where

  docColor    = PP.pretty stringColor
  stringColor = (abbreviateColor color) & fromMaybe ""

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pColor'@
instance Parse Color where
  parser = pColor

--------------------------------------------------

{- | Parses:

* color abbreviations
* english color words
* case-insensitively

Inverts 'ppColor'.

-}

pColor :: MTGParsing m => m Color
pColor = do

  pAssoc cs <?> "Color"

  where

  cs :: Assoc Color
  cs = csLower <> csUpper

  csLower = csUpper <&> (bimap Text.toLower id)

  csUpper =

    [ "W" -: White
    , "U" -: Blue
    , "B" -: Black
    , "R" -: Red
    , "G" -: Green

    , "White" -: White
    , "Blue"  -: Blue
    , "Black" -: Black
    , "Red"   -: Red
    , "Green" -: Green
    ]

--------------------------------------------------

-- | @≡ 'pColor'@

parseColor :: (MonadThrow m) => String -> m Color
parseColor = runParser 'Color pColor 

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Color

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------