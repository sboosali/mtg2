{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Artist where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

newtype Artist = Artist Text
 
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

makePrisms ''Artist

--------------------------------------------------

-- | @≡ 'unknownArtist'@
instance Default Artist where def = Artist ""

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

-- | @≡ ""@
unknownArtist :: Artist
unknownArtist = Artist ""

--------------------------------------------------

_Quinton_Hoover :: Artist
_Quinton_Hoover = "Quinton Hoover"

_Rebecca_Guay :: Artist
_Rebecca_Guay = "Rebecca Guay"

_Terese_Nielsen :: Artist
_Terese_Nielsen = "Terese Nielsen"

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------