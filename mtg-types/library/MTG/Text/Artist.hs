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
-- Imports ---------------------------------------
--------------------------------------------------

--import qualified "text" Data.Text as Text

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
pattern UnknownArtist :: Artist
pattern UnknownArtist = Artist ""

--------------------------------------------------

-- | @≡ "Quinton Hoover"@
pattern QuintonHoover :: Artist
pattern QuintonHoover = Artist "Quinton Hoover"

-- | @≡ "Rebecca Guay"@
pattern RebeccaGuay :: Artist
pattern RebeccaGuay = Artist "Rebecca Guay"

-- | @≡ "Terese Nielsen"@
pattern TereseNielsen :: Artist
pattern TereseNielsen = Artist "Terese Nielsen"

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppArtist'@

instance Pretty Artist where pretty = ppArtist def

--------------------------------------------------

{- | Pretty-Prist an artist's name.

== Examples

>>> show (ppArtist PrettyConfig{ unicode = Enable } QuintonHoover) == "🖌 Quinton Hoover"
True

>>> show (ppArtist PrettyConfig{ unicode = Enable } QuintonHoover)
"🖌 Quinton Hoover"

-}

ppArtist :: PrettyConfig -> Artist -> Doc i
ppArtist PrettyConfig{..} (Artist t) =

  prefix <> pretty t

  where

  prefix = case unicode of

    Enable  -> pretty LOWER_LEFT_PAINTBRUSH <> " "
    Disable -> mempty

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