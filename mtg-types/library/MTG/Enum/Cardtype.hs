{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Cardtype where

import MTG.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Cardtype = Cardtype Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

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
