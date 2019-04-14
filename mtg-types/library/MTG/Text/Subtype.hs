
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Text.Subtype where

import MTG.Types.Prelude

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

newtype Subtype = Subtype Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

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