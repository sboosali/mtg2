{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

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
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Color where
  fromString = (coerce . fromString)

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
-- Pretty ----------------------------------------
--------------------------------------------------

instance Pretty Color where

  pretty = prettyColor

--------------------------------------------------

prettyColor :: Color -> PP.Doc i
prettyColor color0 = braced color1
  where

  color1 = (abbreviateColor color0) & fromMaybe ""

--------------------------------------------------

abbreviateColor :: Color -> Maybe String
abbreviateColor (Color s0) = go s1
  where

  s1 = T.toLower s0

  go = \case

    "white"     -> Just "w"
    "blue"      -> Just "u"
    "black"     -> Just "b"
    "red"       -> Just "r"
    "green"     -> Just "g"
    "colorless" -> Just "c"

    "w"         -> Just "w"
    "u"         -> Just "u"
    "b"         -> Just "b"
    "r"         -> Just "r"
    "g"         -> Just "g"
    "c"         -> Just "c"

    _           -> Nothing

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------