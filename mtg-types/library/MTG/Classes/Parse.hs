--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

{-| 

== Implementation

The 'CharParsing' @class@ from the @parser@ package has @instance@s for:

* @attoparsec@ — used by @mtg-json@ (via the @aeson@ package).
* @trifecta@ — used for pretty-printed parse-errors (e.g. @ANSI@ color codes, “caret diagnostics”).
* @base@'s "Text.Read" — useful for simple and\/or standalone examples (e.g. in @doctest@s).

-}

module MTG.Classes.Parse

  ( Parse(..)

  , Parsing(..)
  , CharParsing(..)
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "parser" Text.Parser.Combinators ( Parsing(..) )
import "parser" Text.Parser.Char        ( CharParsing(..) )

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
-- Classes ---------------------------------------
--------------------------------------------------

{-| Canonical parsing for @mtg-types@.

== Implementation

These typeclasses' methods are available 'parser' implementations:

* 'CharParsing'
* 'Parsing'
* 'Alternative'
* 'Applicative'
* 'Functor'

-}

class Parse a where

  parser :: CharParsing m => m a

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------