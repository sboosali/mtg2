
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Subtype where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Subtype = Subtype Text
 
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Subtype

--------------------------------------------------

--------------------------------------------------

{-

data Subtype
 = SpellSubtype         SpellType
 | LandSubtype          LandType
 | ArtifactSubtype      ArtifactType
 | EnchantmentSubtype   EnchantmentType
 | CreatureSubtype      CreatureType
 | PlaneswalkerSubtype  PlaneswalkerType

-}