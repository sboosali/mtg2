--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}

--------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-# LANGUAGE PatternSynonyms            #-}

--------------------------------------------------

{-| 'Border' is the color (unrelated to "MTG.Text.Color") of a /Magic: The Gathering/ card's border.

-}

module MTG.Text.Border where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

newtype Border = Border

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @= 'toBorder'@
instance IsString Border where
  fromString = fromString > toBorder

--------------------------------------------------

-- | @= 'BlackBorder'@
instance Default Border where def = BlackBorder

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

pattern Borderless :: Border
pattern Borderless = "borderless"

-- | Most cards are block-bordered.

pattern BlackBorder :: Border
pattern BlackBorder = "black"

pattern WhiteBorder :: Border
pattern WhiteBorder = "white"

pattern SilverBorder :: Border
pattern SilverBorder = "silver"

pattern GoldBorder :: Border
pattern GoldBorder = "gold"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

toBorder :: Text -> Border
toBorder = munge > Border
  where

  munge = Text.toLower

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Border

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------