{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Legality where

import MTG.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Legality = Legality Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

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
