{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Name where

import MTG.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype CardName = CardName Text
 
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''CardName

--------------------------------------------------



--------------------------------------------------
