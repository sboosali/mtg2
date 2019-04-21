
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

-}

module MTG.Text.Cardtype where

import MTG.Types.Prelude

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

newtype Cardtype = Cardtype Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Cardtype

 -- | CreatureCardtype      CreatureType
 -- | PlaneswalkerCardtype  PlaneswalkerType
