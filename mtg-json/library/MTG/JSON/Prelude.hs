--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Re-export:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).
* the "MTG.Types.Prelude" @module@ — the @mtg-types@ package's custom prelude.

-}

module MTG.JSON.Prelude

  ( module EXPORT
  , module MTG.Types
--, module MTG.JSON.Prelude
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "mtg-types" MTG.Types

--------------------------------------------------

import "spiros" Prelude.Spiros                     as EXPORT

--------------------------------------------------

import "prettyprinter" Data.Text.Prettyprint.Doc   as EXPORT ( Pretty(..) )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import           "aeson" Data.Aeson                as EXPORT ( FromJSON(..), ToJSON(..) )
import           "aeson" Data.Aeson                as EXPORT ( (.:), (.:?), (.:!), (.!=) )

--------------------------------------------------

import           "uuid-types" Data.UUID.Types      as EXPORT ( UUID )

--------------------------------------------------

import           "attoparsec" Data.Attoparsec.Text as EXPORT ( Parser )

--------------------------------------------------

--import qualified "formatting" Formatting as Format

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import           "vector" Data.Vector              as EXPORT ( Vector )

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------