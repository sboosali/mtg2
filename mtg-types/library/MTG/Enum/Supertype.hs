{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Supertype where

import MTG.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Supertype = Supertype Text
 
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Supertype

--------------------------------------------------

basicSupertype :: Supertype
basicSupertype = "Basic"

legendarySupertype :: Supertype
legendarySupertype = "Legendary"

snowSupertype :: Supertype
snowSupertype = "Snow"

ongoingSupertype :: Supertype
ongoingSupertype = "Ongoing"

worldSupertype :: Supertype
worldSupertype = "World"
 
--------------------------------------------------
