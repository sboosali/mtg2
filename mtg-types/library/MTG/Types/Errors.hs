--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------
--------------------------------------------------

{-| Custom @Exception@s for @mtg-types@

== Types

* 'ParseError' — for syntax errors while parsing "MTG.Classes.Parse.Parse" instances.

-}

module MTG.Types.Errors where

--------------------------------------------------
-- Imports: Internal -----------------------------
--------------------------------------------------

--import MTG.Classes.Parse
import MTG.Classes.Print

import MTG.Classes.Prelude

--------------------------------------------------
-- Imports: External -----------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc as PP

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

data ParseError = ParseError

  { getParseErrors :: [MTGDocument]
  }

  deriving stock    (Generic)

--------------------------------------------------

instance Exception ParseError where

  -- | @'displayException' \@'ParseError' ≡ 'displayParseError'@

  displayException = displayParseError

--------------------------------------------------

{-| @≡ 'displayParseError' @ -}

instance Show ParseError where

  -- | @show \@'ParseError' ≡ 'displayParseError'@

  showsPrec precedence x = showParen (precedence >= maximumPrecedence) (s ++)
    where

    s :: String
    s = displayParseError x

--------------------------------------------------

-- | @≡ ('++')@
instance Semigroup ParseError where

  ParseError xs <> ParseError ys = ParseError (xs <> ys)

--------------------------------------------------

-- | @≡ []@
instance Monoid ParseError where

  mempty = ParseError []

--------------------------------------------------

-- | @≡ 'parseError'@

instance IsString ParseError where

  fromString = parseError

--------------------------------------------------

-- | @≡ 'parseErrors'@

instance IsList ParseError where

  type Item (ParseError) = MTGDocument

  fromList = ParseError
  toList   = getParseErrors

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| A single (string) error.

== Signatures

@
'ParseError' :: ['MTGDocument'] -> 'ParseError'
'parseError' :: String        -> 'ParseError'
@

-}

parseError :: String -> ParseError
parseError

  = pretty
  > (: [])
  > ParseError

--------------------------------------------------

{-| Multiple (string) errors.

== Signatures

@
'ParseError' :: ['MTGDocument'] -> 'ParseError'
'parseErrors' :: [String]     -> 'ParseError'
@

-}

parseErrors :: [String] -> ParseError
parseErrors

  = fmap pretty
  > ParseError

--------------------------------------------------

{-| Pretty-Print a 'ParseError'.

== Examples

>>> Prelude.putStrLn $ displayParseError (ParseError ["doesn't parse", "doesn't validate"])
doesn't parse
doesn't validate

-}

displayParseError :: ParseError -> String
displayParseError

  = getParseErrors
  > PP.vsep
  > renderString_MTGDocument

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{-$setup

>>> :set -XPackageImports
>>> :set -XOverloadedStrings

-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

TODO:

 >>> :set -XOverloadedStrings
 >>> Prelude.putStrLn (Prelude.show ("unparseable" :: ParseError))
 [ParseError] Can't parse <<< "unparseable" >>>.

>>> Prelude.putStrLn $ displayParseError (ParseError [])

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------