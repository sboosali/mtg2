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

  , module MTG.Types.Errors
  , module MTG.Classes.Print
  , module MTG.Classes.Parse
  , module MTG.Classes.Prelude

  , module EXPORT
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import MTG.Types.Errors

import MTG.Classes.Print
import MTG.Classes.Parse

import MTG.Classes.Prelude

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

--------------------------------------------------

import "parsers" Text.Parser.Combinators as EXPORT ( Parsing( (<?>) ))
import "parsers" Text.Parser.Char        as EXPORT ( CharParsing )
import "parsers" Text.Parser.Token       as EXPORT ( TokenParsing )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text   as PP.Text

--------------------------------------------------

import "spiros" Prelude.Spiros.GUI

--------------------------------------------------

import qualified "base" Text.ParserCombinators.ReadP as Read
import           "base" Text.ParserCombinators.ReadP ( ReadP )

--------------------------------------------------

import           "base" GHC.Stack.Types (HasCallStack)

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Wraps 'readP_to_S'.

runParser
  :: forall m a.
     ( MonadThrow m
     )
  => Name -> (forall p. (MTGParsing p) => p a)
  -> (String -> m a)

runParser name p = go
  where

  go :: String -> m a
  go =

    let
      p' :: ReadP a
      p' = p

      s = displayName name
      e = parseError s
    in

      Read.readP_to_S p' > fmap fst > throwListM e

-- readP_to_S :: ReadP a -> ReadS a
-- readP_to_S :: ReadP a -> String -> [(a,String)]

--------------------------------------------------

-- | Aliases 'renderText'
renderText :: PP.SimpleDocStream i -> Text
renderText = PP.Text.renderStrict

--------------------------------------------------

{- | 

@
instance 'IsString' XYZ where
  fromString = 'fromString_MonadThrow' parseXYZ
@

-}

fromString_MonadThrow
  :: forall a.

     ( HasCallStack
     )

  => (forall m. (MonadThrow m) => String -> m a)

  -> (String -> a)

fromString_MonadThrow pM = pI
  where

  pI :: String -> a
  pI = pE > either mkError id

  pE :: String -> Either SomeException a
  pE = pM

  -- ⇑ instantiate « m » as « Either SomeException ».

  mkError :: forall x. (HasCallStack) => SomeException -> x
  mkError = displayException > error

  -- ⇑ « error :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => [Char] -> a ».

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------