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

import MTG.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
--import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "text" Data.Text as T

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|

-}

newtype Color = Color

  Text

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

makePrisms ''Color

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

toColors :: Maybe [Text] -> [Color]
toColors = maybe [] (fmap Color)

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

colorless :: Color
colorless = "Colorless"

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

-- | @≡ "Colorless"@

pattern Colorless :: Color
pattern Colorless = "Colorless"

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
abbreviateColor (Color s0) = T.toUpper <$> (go s1)
  where

  s1 = T.toLower s0

  go = \case

    "white"     -> Just "W"
    "blue"      -> Just "U"
    "black"     -> Just "B"
    "red"       -> Just "R"
    "green"     -> Just "G"
    "colorless" -> Just "C"

    "w"         -> Just "W"
    "u"         -> Just "U"
    "b"         -> Just "B"
    "r"         -> Just "R"
    "g"         -> Just "G"
    "c"         -> Just "C"

    _           -> Nothing

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------