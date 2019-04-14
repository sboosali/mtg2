--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-| 'Artist' is the name of an artist.

-}

module MTG.Text.Artist where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import "lens" Control.Lens (makePrisms)

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

newtype Artist = Artist Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'noArtist'@
instance Default Artist where def = Artist ""

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

knownArtists :: [Artist]
knownArtists =

  [ QuintonHoover
  , RebeccaGuay
  , TereseNielsen
  ] 

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

-- | @≡ ""@
pattern NoArtist :: Artist
pattern NoArtist = Artist ""

--------------------------------------------------

pattern QuintonHoover :: Artist
pattern QuintonHoover = "Quinton Hoover"

pattern RebeccaGuay :: Artist
pattern RebeccaGuay = "Rebecca Guay"

pattern TereseNielsen :: Artist
pattern TereseNielsen = "Terese Nielsen"

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Artist

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------