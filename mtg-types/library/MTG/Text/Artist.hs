--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-| 'Artist' is the name of an artist who has illustrated a /Magic: The Gathering/ card.

-}

module MTG.Text.Artist where

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

newtype Artist = Artist

  Text
 
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
pattern QuintonHoover = Artist "Quinton Hoover"

pattern RebeccaGuay :: Artist
pattern RebeccaGuay = Artist "Rebecca Guay"

pattern TereseNielsen :: Artist
pattern TereseNielsen = Artist "Terese Nielsen"

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppArtist'@

instance Pretty Artist where

  pretty = ppArtist

--------------------------------------------------

{- | -}

ppArtist :: Artist -> Doc i
ppArtist (Artist t) =

  pretty t

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pArtist'@

instance Parse Artist where

  parser = pArtist

--------------------------------------------------

{- | -}

pArtist :: (MTGParsing m) => m Artist
pArtist = Artist <$> pFreeText <?> "Artist"

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Artist

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------