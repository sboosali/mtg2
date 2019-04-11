--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{-# LANGUAGE RankNTypes #-}

--------------------------------------------------

{- | Custom @Prelude@, for the @mtg-types@ package.

Re-exports:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).
* the 'Pretty' @class@ — Canonical, humanable-readable /printing/ for all @data@types in @mtg-types@ (from the @prettyprinter@ package).
* the 'Parse' @class@ — Canonical, humanable-readable /parsing/ for all @data@types in @mtg-types@ (defined in @mtg-types@ itself, uses 'CharParsing' from the @parser@ package).
* the 'Text' @data@type — /strict/ (from the @text@ package).

-}

module MTG.Types.Prelude

  ( module MTG.Types.Prelude

  , module MTG.Classes.Print
  , module MTG.Classes.Parse
  , module MTG.Classes.Prelude

  , module EXPORT
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
-- Exports ---------------------------------------
--------------------------------------------------

import "prettyprinter" Data.Text.Prettyprint.Doc               as EXPORT ( Pretty(..) )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as EXPORT ( renderString )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text   as EXPORT ( renderStrict )

--------------------------------------------------

import "parsers" Text.Parser.Combinators as EXPORT ( Parsing( (<?>) ))
import "parsers" Text.Parser.Char        as EXPORT ( CharParsing )
import "parsers" Text.Parser.Token       as EXPORT ( TokenParsing )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "base" Text.ParserCombinators.ReadP as Read
import           "base" Text.ParserCombinators.ReadP ( ReadP )

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Wraps 'readP_to_S'.

runParser
  :: forall m a.
     ( MonadThrow m
     )
  => (forall p. (MTGParsing p) => p a)
  -> (String -> m a)

runParser p = go
  where

  go :: String -> m a
  go =

    let
      p' :: ReadP a
      p' = p
    in
      Read.readP_to_S p' > fmap fst > throwListM

-- readP_to_S :: ReadP a -> ReadS a
-- readP_to_S :: ReadP a -> String -> [(a,String)]

--------------------------------------------------

-- | Wraps 'renderString'.

runPrinter :: Doc i -> String
runPrinter = PP.layoutSmart PP.defaultLayoutOptions > PP.String.renderString
  
--------------------------------------------------

-- | Aliases 'renderText'

renderText :: PP.SimpleDocStream i -> Text
renderText = renderStrict

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------