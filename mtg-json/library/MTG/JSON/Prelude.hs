--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Re-export:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).
* the 'Parse' @class@ — Canonical, humanable-readable /parsing/ for all @data@types in @mtg-types@ (defined in @mtg-types@ itself).

-}

module MTG.Types.Prelude

  ( module EXPORT
  , module MTG.Types.Parse
  ) where








--------------------------------------------------

module MTG.JSON.Prelude

  ( module EXPORT
--, module MTG.Types.Prelude
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "mtg-types" MTG.Types.Prelude as EXPORT

--------------------------------------------------

import "spiros" Prelude.Spiros as EXPORT

--------------------------------------------------

import "enumerate" Enumerate as EXPORT

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