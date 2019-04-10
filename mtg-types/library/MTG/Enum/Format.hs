{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Format where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Format = Format Text
 
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

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
