--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- Types -----------------------------------------
--------------------------------------------------

newtype Border = Border

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @= 'BlackBorder'@
instance Default Border where def = BlackBorder

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

-- | Most cards are block-bordered.

pattern BlackBorder :: Border
pattern BlackBorder = "black"

pattern WhiteBorder :: Border
pattern WhiteBorder = "white"

pattern SilverBorder :: Border
pattern SilverBorder = "silver"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

toBorder :: Text -> Border
toBorder = Border

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Border

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------