{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| 

-}

module MTG.JSON.UUID where

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

newtype UUID = UUID

  Text

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable,Pretty)

--------------------------------------------------

makePrisms ''UUID

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------