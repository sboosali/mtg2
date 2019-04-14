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
  , module MTG.Types.Prelude
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "mtg-types" MTG.Types.Prelude

--------------------------------------------------

import "spiros" Prelude.Spiros as EXPORT

--------------------------------------------------

--import "enumerate" Enumerate as EXPORT

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "prettyprinter" Data.Text.Prettyprint.Doc as EXPORT ( Pretty(..) )

--------------------------------------------------

import "attoparsec" Data.Attoparsec.Text as EXPORT ( Parser(..) )

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------