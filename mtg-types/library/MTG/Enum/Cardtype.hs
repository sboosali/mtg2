{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Cardtype where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Cardtype = Cardtype Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Cardtype

-- | @= 'instantType'@
instance Default Cardtype where def = instantType

--------------------------------------------------

instantType :: Cardtype
instantType = "Instant"

sorceryType :: Cardtype
sorceryType = "Sorcery"

landType :: Cardtype
landType = "Land"

artifactType :: Cardtype
artifactType = "Artifact"

enchantmentType :: Cardtype
enchantmentType = "Enchantment"

creatureType :: Cardtype
creatureType = "Creature"

planeswalkerType :: Cardtype
planeswalkerType = "Planeswalker"

conspiracyType :: Cardtype
conspiracyType = "Conspiracy"
 
--------------------------------------------------
