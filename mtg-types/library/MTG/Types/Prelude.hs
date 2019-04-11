--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Re-export:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).
* the 'Pretty' @class@ — Canonical, humanable-readable /printing/ for all @data@types in @mtg-types@ (from the @prettyprinter@ package).
* the 'Parse' @class@ — Canonical, humanable-readable /parsing/ for all @data@types in @mtg-types@ (defined in @mtg-types@ itself, uses 'CharParsing' from the @parser@ package).
* the 'Text' @data@type — /strict/ (from the @text@ package).

-}

module MTG.Types.Prelude

  ( module EXPORT
  , module MTG.Classes.Parse
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros as EXPORT

--------------------------------------------------

import "enumerate" Enumerate as EXPORT

--------------------------------------------------

import MTG.Classes.Parse

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "prettyprinter" Data.Text.Prettyprint.Doc as EXPORT ( Pretty(..) )

--------------------------------------------------

--import "th-lift-instances" Instances.TH.Lift()

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc as PP

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------