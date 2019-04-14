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

--import qualified "parsers" Text.Parser.Combinators as P
import qualified "parsers" Text.Parser.Char        as P
import qualified "parsers" Text.Parser.Token       as P

import "parsers" Text.Parser.Combinators ( Parsing( (<?>) ))
import "parsers" Text.Parser.Char        ( CharParsing )
import "parsers" Text.Parser.Token       ( TokenParsing )

--------------------------------------------------

import qualified "charset" Data.CharSet as CharSet
import           "charset" Data.CharSet ( CharSet )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------

import qualified "base" Data.Char as Char

--------------------------------------------------
-- Constraints -----------------------------------
--------------------------------------------------

-- | a @ConstraintKind@.
--
-- (See <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstraintKinds @-XConstraintKinds@>.)

type MTGParsing m =

  ( TokenParsing m
  )

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
-- Text parsers...

{-| Parse a symbol token.

See 'pText'.

== Examples

>>> runParser 'pSymbol pSymbol "{αàéñ①∞∅↑•✔✘❓}"

== Definition

@
≡ 'P.braces' 'pText'
@

-}

pSymbolText
  :: forall m.
     ( TokenParsing m
     )
  => m Text

pSymbolText = P.braces pText

--------------------------------------------------

{-| Parse a token.

See 'pSymbolTextChar'.

i.e. a sequence of characters which satisfy
`Char.isPrint` but not `Char.isSpace`
(with a few exceptions).

i.e.:

* no whitespace.
* no unprintable characters.
* any letter
* any digit
* any “symbol”
* any “punctuation”
* any “separator”

In particular,`pText` accepts many Unicode characters, like:

* @'α'@
* @'à'@
* @'é'@
* @'ñ'@
* @'①'@
* @'∞'@
* @'∅'@
* @'↑'@
* @'•'@
* @'✔'@
* @'✘'@
* @'❓'@

== Examples

>>> 

-}

pText
  :: forall m.
     ( CharParsing m
     )
  => m Text

pText = Text.pack <$> some pSymbolTextChar

--------------------------------------------------

{-| Parse /free text/.

i.e. @pFreeText@ parses:

* yes printable.

See 'pFreeChar'.

== Examples

Parses whitespace and Unicode:

>>> (runParser 'pFreeText pFreeText "日本語")
"日本語"
>>> (runParser 'pFreeText pFreeText "Quinton Hoover")
"Quinton Hoover"

(i.e. including non-ASCII characters.)

== Definition

@
≡ 'some' 'pFreeChar'
@

-}

pFreeText
  :: forall m.
     ( CharParsing m
     )
  => m Text

pFreeText = Text.pack <$> some pFreeChar

--------------------------------------------------

{-| Parse /free text/, with the given exceptions.

i.e. @pUnfreeText cs@ parses:

* yes printable.
* not @p@ (where @p@ parses any @c@ in @cs@).

See 'pUnfreeChar'.

== Examples

>>> Nothing <- runParser 'pUnfreeText (pUnfreeText " _") "free_text")
>>> Nothing <- runParser 'pUnfreeText (pUnfreeText " _") "free text")
>>> runParser 'pUnfreeText (pUnfreeText "-_") "free text")
"free text"

== Definition

@
≡ 'some' 'pUnfreeChar'
@

-}

pUnfreeText
  :: forall m.
     ( CharParsing m
     )
  => [Char]
  -> m Text

pUnfreeText cs = Text.pack <$> some (pUnfreeChar cs)

--------------------------------------------------
-- Char parsers...
--------------------------------------------------

{- | Parse a /free text/ character.

i.e. @pText@ parses:

* yes printable.

-}

pFreeChar
  :: forall m.
     ( CharParsing m
     )
  => m Char

pFreeChar = P.satisfy predicate
  where

  predicate c = Char.isPrint c

--------------------------------------------------

{- | Parse a /free text/ character, unless it's in the given character set.

i.e. @pUnfreeChar cs@ parses:

* yes printable.
* not one of @cs@

-}

pUnfreeChar
  :: forall m.
     ( CharParsing m
     )
  => [Char]
  -> m Char

pUnfreeChar cs = P.satisfy predicate
  where

  predicate c

    = Char.isPrint c
   && not (c `CharSet.member` cs')

  -- cache the character-set:

  cs' :: CharSet
  cs' = CharSet.fromList cs

--------------------------------------------------

{- | Parse a /token/ character.

i.e.:

* yes printable.
* not whitespace.
* not a /reserved/ separator (e.g. not @\'}\'@, the right-brace).

== Implementation

c.f. `Char.generalCategory`:

>>> Char.generalCategory ' '
Space
>>> --
>>> Char.generalCategory '{'
OpenPunctuation
>>> Char.generalCategory '}'
ClosePunctuation
>>> Char.generalCategory '"'
OtherPunctuation
>>> Char.generalCategory '\''
OtherPunctuation
>>> --
>>> Char.generalCategory '+'
MathSymbol
>>> Char.generalCategory '='
MathSymbol
>>> Char.generalCategory '<'
MathSymbol
>>> Char.generalCategory '>'
MathSymbol
>>> Char.generalCategory '-'
DashPunctuation
>>> Char.generalCategory '^'
ModifierSymbol
>>> --
>>> Char.generalCategory '_'
ConnectorPunctuation
>>> Char.generalCategory '`'
ModifierSymbol
>>> Char.generalCategory '/'
OtherPunctuation
>>> Char.generalCategory '.'
OtherPunctuation
>>> Char.generalCategory ':'
OtherPunctuation
>>> Char.generalCategory ';'
OtherPunctuation
>>> Char.generalCategory ','
OtherPunctuation
>>> Char.generalCategory '|'
MathSymbol
>>> --
>>> Char.generalCategor '0'
DecimalNumber
>>> Char.generalCategory 'a'
LowercaseLetter
>>> Char.generalCategory 'A'
UppercaseLetter

-}

pSymbolTextChar
  :: forall m.
     ( CharParsing m
     )
  => m Char

pSymbolTextChar = P.satisfy predicate
  where

  predicate c

    = Char.isPrint c
   && not (Char.isSpace c)
   && not (isAsciiBrace c)
   && not (isAsciiQuote c)

  isAsciiQuote c
    = c == '"'
   || c == '\''

  isAsciiBrace c
    = c == '{'
   || c == '}'

--------------------------------------------------
-- Utilities parsers...
--------------------------------------------------

{-| Parse an @Enum@, given an /association list/.

Fails via 'empty' or 'P.unexpected'.

== Examples

>>> namedBools = [ "off"-: False, "0"-: False, "on"-: True, "1"-: True ]
>>> pBool = pAssoc namedBools <?> "Bool"
>>> parseBool = runParser 'pBool pBool
>>> parseBool "on"
True
>>> parseBool "xxx"

-}

pAssoc
  :: forall a m.
     ( CharParsing m
     , Ord a
     )
  => Assoc a
  -> m a

pAssoc kvs = do

  let pKVs = (pPair <$> kvs)

  (foldr (<|>) empty) pKVs

  where

  pPair (k,v) = v <$ P.text k

{-# INLINEABLE pAssoc #-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-
--------------------------------------------------

oneOfSet (CharSet True _ ints)  = satisfy (\c -> IntSet.member (fromEnum c) ints)
oneOfSet (CharSet False _ ints) = satisfy (\c -> not (IntSet.member (fromEnum c) ints))

--------------------------------------------------

  data UnicodeCategory = UnicodeCategory String String CharSet String
    deriving (Show, Data, Typeable)

  -- \p{Letter} or \p{Mc}

  unicodeCategories :: [UnicodeCategory]
  unicodeCategories =

    [ UnicodeCategory     "Letter"           "L"  letter          "any kind of letter from any language."
    ,     UnicodeCategory "Lowercase_Letter" "Ll" lowercaseLetter "a lowercase letter that has an uppercase variant"
    ,     UnicodeCategory "Uppercase_Letter" "Lu" uppercaseLetter "an uppercase letter that has a lowercase variant"
    ,     UnicodeCategory "Titlecase_Letter" "Lt" titlecaseLetter "a letter that appears at the start of a word when only the first letter of the word is capitalized"
    ,     UnicodeCategory "Letter&"          "L&" letterAnd       "a letter that exists in lowercase and uppercase variants (combination of Ll, Lu and Lt)"
    ,     UnicodeCategory "Modifier_Letter"  "Lm" modifierLetter  "a special character that is used like a letter"
    ,     UnicodeCategory "Other_Letter"     "Lo" otherLetter     "a letter or ideograph that does not have lowercase and uppercase variants"

    , UnicodeCategory     "Mark"                   "M"  mark                 "a character intended to be combined with another character (e.g. accents, umlauts, enclosing boxes, etc.)"
    ,     UnicodeCategory "Non_Spacing_Mark"       "Mn" nonSpacingMark       "a character intended to be combined with another character without taking up extra space (e.g. accents, umlauts, etc.)"
    ,     UnicodeCategory "Spacing_Combining_Mark" "Mc" spacingCombiningMark "a character intended to be combined with another character that takes up extra space (vowel signs in many Eastern languages)"
    ,     UnicodeCategory "Enclosing_Mark"         "Me" enclosingMark        "a character that encloses the character is is combined with (circle, square, keycap, etc.)"

    , UnicodeCategory     "Separator"           "Z"  separator          "any kind of whitespace or invisible separator"
    ,     UnicodeCategory "Space_Separator"     "Zs" space              "a whitespace character that is invisible, but does take up space"
    ,     UnicodeCategory "Line_Separator"      "Zl" lineSeparator      "line separator character U+2028"
    ,     UnicodeCategory "Paragraph_Separator" "Zp" paragraphSeparator "paragraph separator character U+2029"

    , UnicodeCategory     "Symbol"          "S" symbol          "math symbols, currency signs, dingbats, box-drawing characters, etc."
    ,     UnicodeCategory "Math_Symbol"     "Sm" mathSymbol     "any mathematical symbol"
    ,     UnicodeCategory "Currency_Symbol" "Sc" currencySymbol "any currency sign"
    ,     UnicodeCategory "Modifier_Symbol" "Sk" modifierSymbol "a combining character (mark) as a full character on its own"
    ,     UnicodeCategory "Other_Symbol"    "So" otherSymbol     "various symbols that are not math symbols, currency signs, or combining characters"

    , UnicodeCategory     "Number"               "N"  number        "any kind of numeric character in any script"
    ,     UnicodeCategory "Decimal_Digit_Number" "Nd" decimalNumber "a digit zero through nine in any script except ideographic scripts"
    ,     UnicodeCategory "Letter_Number"        "Nl" letterNumber  "a number that looks like a letter, such as a Roman numeral"
    ,     UnicodeCategory "Other_Number"         "No" otherNumber   "a superscript or subscript digit, or a number that is not a digit 0..9 (excluding numbers from ideographic scripts)"

    , UnicodeCategory     "Punctuation"           "P"  punctuation          "any kind of punctuation character"
    ,     UnicodeCategory "Dash_Punctuation"      "Pd" dashPunctuation      "any kind of hyphen or dash"
    ,     UnicodeCategory "Open_Punctuation"      "Ps" openPunctuation      "any kind of opening bracket"
    ,     UnicodeCategory "Close_Punctuation"     "Pe" closePunctuation     "any kind of closing bracket"
    ,     UnicodeCategory "Initial_Punctuation"   "Pi" initialQuote         "any kind of opening quote"
    ,     UnicodeCategory "Final_Punctuation"     "Pf" finalQuote           "any kind of closing quote"
    ,     UnicodeCategory "Connector_Punctuation" "Pc" connectorPunctuation "a punctuation character such as an underscore that connects words"
    ,     UnicodeCategory "Other_Punctuation"     "Po" otherPunctuation     "any kind of punctuation character that is not a dash, bracket, quote or connector"

    , UnicodeCategory     "Other"       "C"  other       "invisible control characters and unused code points"
    ,     UnicodeCategory "Control"     "Cc" control     "an ASCII 0x00..0x1F or Latin-1 0x80..0x9F control character"
    ,     UnicodeCategory "Format"      "Cf" format      "invisible formatting indicator"
    ,     UnicodeCategory "Private_Use" "Co" privateUse  "any code point reserved for private use"
    ,     UnicodeCategory "Surrogate"   "Cs" surrogate   "one half of a surrogate pair in UTF-16 encoding"
    ,     UnicodeCategory "Unassigned"  "Cn" notAssigned "any code point to which no character has been assigned.properties

    ]

--------------------------------------------------

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------