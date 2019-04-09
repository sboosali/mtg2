{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Format where

import MTG.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Format = Format Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Format

-- | @= 'vintageFormat'@
instance Default Format where def = vintageFormat

--------------------------------------------------

standardFormat :: Format
standardFormat = "standard"

blockFormat :: Format
blockFormat = "block"

extendedFormat :: Format
extendedFormat = "extended"

vintageFormat :: Format
vintageFormat = "vintage"

classicFormat :: Format
classicFormat = "classic"

legacyFormat :: Format
legacyFormat = "legacy"

modernFormat :: Format
modernFormat = "modern"

commanderFormat :: Format
commanderFormat = "commander"

--------------------------------------------------
