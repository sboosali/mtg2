--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Custom @Prelude@, for the @mtg-types@ package.

Re-exports:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).
* the 'Pretty' @class@ — Canonical, humanable-readable /printing/ for all @data@types in @mtg-types@ (from the @prettyprinter@ package).
* the 'Parse' @class@ — Canonical, humanable-readable /parsing/ for all @data@types in @mtg-types@ (defined in @mtg-types@ itself, uses 'CharParsing' from the @parser@ package).
* the 'Text' @data@type — /strict/ (from the @text@ package).

-}

module MTG.Types.Prelude

  ( module EXPORT
  , module MTG.Classes.Print
  , module MTG.Classes.Parse
  , module MTG.Classes.Prelude
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import MTG.Classes.Print
import MTG.Classes.Parse

import MTG.Classes.Prelude (Assoc)

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros as EXPORT

--------------------------------------------------

import "enumerate" Enumerate as EXPORT

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "prettyprinter" Data.Text.Prettyprint.Doc               as EXPORT ( Pretty(..) )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as EXPORT ( renderString )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text   as EXPORT ( renderText )

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------