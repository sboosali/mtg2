{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Legality where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Legality = Legality Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Legality

-- | @= 'legal'@
instance Default Legality where def = legal

--------------------------------------------------

legal :: Legality
legal = "legal"

restricted :: Legality
restricted = "restricted"

banned :: Legality
banned = "banned"

--------------------------------------------------
