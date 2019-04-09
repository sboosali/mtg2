{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Name where

import MTG.Prelude

import Control.Lens (makePrisms)

----------------------------------------

newtype CardName = CardName Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''CardName

----------------------------------------

----------------------------------------
