{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| 

== Examples

>>> pretty blue
{U}

-}

module MTG.Enum.Color where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "attoparsec" Data.Attoparsec.Text as P

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|

-}

newtype Color = Color

  Text

  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

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
-- Constants -------------------------------------
--------------------------------------------------

white :: Color
white = "White"

blue :: Color
blue = "Blue"

black :: Color
black = "Black"

red :: Color
red = "Red"

green :: Color
green = "Green"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

toColors :: Maybe [Text] -> [Color]
toColors = maybe [] (fmap Color)

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'PP.braces' . 'abbreviateColor'@

instance Pretty Color where

  pretty = prettyColor

--------------------------------------------------

prettyColor :: Color -> PP.Doc i
prettyColor color = PP.braces docColor
  where

  docColor    = PP.pretty stringColor
  stringColor = (abbreviateColor color) & fromMaybe ""

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
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Color

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------