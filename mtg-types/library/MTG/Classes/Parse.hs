--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}

--------------------------------------------------
--------------------------------------------------

{-| 

== Implementation

The @*Parsing@ @class@es from the @parser@ package (i.e. 'Parsing', 'CharParsing', and 'TokenParsing')
have @instance@s for:

* @attoparsec@ — used by @mtg-json@ (via the @aeson@ package).
* @trifecta@ — used for pretty-printed parse-errors (e.g. @ANSI@ color codes, “caret diagnostics”).
* @base@'s "Text.Read" — useful for simple and\/or standalone examples (e.g. in @doctest@s).

-}

module MTG.Classes.Parse

  ( module MTG.Classes.Parse
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Classes.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "parsers" Text.Parser.Combinators as P
import qualified "parsers" Text.Parser.Char        as P
import qualified "parsers" Text.Parser.Token       as P

import "parsers" Text.Parser.Combinators ( Parsing )
import "parsers" Text.Parser.Char        ( CharParsing )
import "parsers" Text.Parser.Token       ( TokenParsing )

--------------------------------------------------
-- Constraints -----------------------------------
--------------------------------------------------

-- | a @ConstraintKind@.
--
-- (See <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstraintKinds @-XConstraintKinds@>.)

type MTGParsing m = (TokenParsing m)

--------------------------------------------------
-- Classes ---------------------------------------
--------------------------------------------------

{-| Canonical parsing for @mtg-types@.

== Implementation

These typeclasses' methods are available to 'parser' implementations
(via 'MTGParsing'):

* 'TokenParsing'
* 'CharParsing'
* 'Parsing'
* 'Alternative'
* 'Applicative'
* 'Functor'

-}

class Parse a where

  parser :: MTGParsing m => m a

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| Parse an @Enum@, given an /association list/.

Fails via 'empty' or 'P.unexpected'.

== Examples

>>> (<?>) :: m a -> String -> m a

-}

pAssoc
  :: (CharParsing m)
  => Assoc a
  -> m a

pAssoc kvs = do

  let pKVs = (pPair <$> kvs)

  (foldr (<|>) empty) pKVs

  where

  pPair (k,v) = v <$ P.text k

{-# INLINEABLE pAssoc #-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------