{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| 

-}

module MTG.JSON.CMC where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.JSON.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
--import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "text" Data.Text as T

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

newtype CMC = CMC

  Natural

  deriving stock    (Show,Read)
  deriving stock    (Lift,Generic)
  deriving newtype  (Num)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @= '(+)'@
instance Semigroup CMC where (<>)   = coerce (+)

-- | @= 0@
instance Monoid    CMC where mempty = coerce 0

--------------------------------------------------

-- | @= 'defaultCMC'@
instance Default CMC where def = defaultCMC

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------



--------------------------------------------------

{- | @= 0@

By default, cards with no mana cost (like lands) have zero /converted/ mana cost.

-}

defaultCMC :: CMC
defaultCMC = mempty

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''CMC

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------