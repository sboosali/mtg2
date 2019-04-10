{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Symbol where

import MTG.Prelude

--import MTG.Enum.Mana

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Symbol = Symbol Text
 
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Symbol

--------------------------------------------------

tapSymbol :: Symbol
tapSymbol = "T"

untapSymbol :: Symbol
untapSymbol = "Q"

--------------------------------------------------

loyaltyActivationSymbol :: Integer -> Symbol
loyaltyActivationSymbol i =
  if i >= 0
  then Symbol $ "+" <> s -- {+0}, {+1}, ...
  else Symbol $ "-" <> s -- {-1}, ...
  where
  s = (fromString . show) (abs i)

--------------------------------------------------