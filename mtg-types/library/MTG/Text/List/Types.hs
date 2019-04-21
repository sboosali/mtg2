--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{- | 

== Examples

Construction:

e.g. 'toTypes':

>>> toTypes [ Blue, Green, Green, Blue ]
Types ["Green","Blue"]
>>> toTypes ["Green","Blue"] == toTypes ["Blue","Green"]
True

Printing (see 'pretty'):

e.g. 'prettyTypes':

>>> pretty Simic
GU

Parsing (see 'parser'):

e.g. 'parseTypes':

>>> parseTypes "UG"
Types ["Green","Blue"]

-}

module MTG.Text.List.Types

  ( module MTG.Text.List.Types
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--import qualified MTG.Enum.Type as Type
import           MTG.Text.Subtype
import           MTG.Text.Cardtype
import           MTG.Text.Supertype

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| Set of 'Color's.

== Constructors

* 'toTypes'

-}

newtype Types = Types

  [Type]

  deriving stock    (Show,Read)
  deriving stock    (Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'toTypes'@

instance IsList Types where
  type Item Types = Color
  fromList = toTypes
  toList   = coerce

--------------------------------------------------

-- | @≡ 'parseTypes'@

instance IsString Types where
  fromString = fromString_MonadThrow parseTypes

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------
-- Guilds (ally-types)...

-- | @≡ ['White', 'Blue']@
pattern Azorius :: Types
pattern Azorius = Types [White, Blue]

-- | @≡ ['Blue', 'Black']@
pattern Dimir :: Types
pattern Dimir = Types [Blue, Black]

-- | @≡ ['Black', 'Red']@
pattern Rakdos :: Types
pattern Rakdos = Types [Black, Red]

-- | @≡ ['Red', 'Green']@
pattern Gruul :: Types
pattern Gruul = Types [Red, Green]

-- | @≡ ['Green', 'White']@
pattern Selesnya :: Types
pattern Selesnya = Types [Green, White]

--------------------------------------------------
-- Guilds (enemy-color)...

-- | @≡ ['White', 'Black']@
pattern Orzhov :: Types
pattern Orzhov = Types [White, Black]

-- | @≡ ['Blue', 'Red']@
pattern Izzet :: Types
pattern Izzet = Types [Blue, Red]

-- | @≡ ['Black', 'Green']@
pattern Golgari :: Types
pattern Golgari = Types [Black, Green]

-- | @≡ ['Red', 'White']@
pattern Boros :: Types
pattern Boros = Types [Red, White]

-- | @≡ ['Green', 'Blue']@
pattern Simic :: Types
pattern Simic = Types [Green, Blue]

--------------------------------------------------
-- Shards...

--------------------------------------------------
-- Wedges...

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

allGuilds :: [Types]
allGuilds =

  [ Azorius
  , Dimir
  , Rakdos
  , Gruul
  , Selesnya
  , Orzhov
  , Izzet
  , Golgari
  , Boros
  , Simic
  ]

--------------------------------------------------
{-
knownTrichromes :: Types
knownTrichromes =

  [ 
  , 
  ]

--------------------------------------------------

knownTetrachromes :: Types
knownTetrachromes =

  [ 
  , 
  ]
-}

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Accessor for 'Types'.

getTypes :: Types -> [Color]
getTypes (Types cs) = cs

--------------------------------------------------

-- | Smart Constructor for 'Types'.

toTypes :: [Color] -> Types
toTypes unsortedTypes = Types sortedTypes
  where

  sortedTypes = sortTypes uniqueTypes
  uniqueTypes = ordNub unsortedTypes

--------------------------------------------------

{- | Wraps `Color.sortMTGTypes`.

Outputs:

* an /idiomatically sorted/ prefix of known types.
* an “unsorted” (i.e. sorted by 'Ord') suffix of unknown types.

-}

sortTypes :: [Color] -> [Color]
sortTypes

  = fmap isColorKnown
  > partitionEithers
  > go

  where

  go ( unknownTypes, knownTypes )
    = sortKnownTypes knownTypes
   ++ sortUnknownTypes unknownTypes

  sortKnownTypes :: [Color.Color] -> [Color]
  sortKnownTypes
    = Color.sortMTGTypes
    > fmap (show > Text.pack > Color)

  sortUnknownTypes :: [Text] -> [Color] 
  sortUnknownTypes
    = sort
    > fmap Color

{-# INLINEABLE sortTypes #-}

--------------------------------------------------

isColorKnown :: Color -> Either Text Color.Color
isColorKnown = \case

  White -> Right Color.White
  Blue  -> Right Color.Blue
  Black -> Right Color.Black
  Red   -> Right Color.Red
  Green -> Right Color.Green

  Color t -> Left t

{-# INLINEABLE isColorKnown #-}

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppTypes'@

instance Pretty Types where

  pretty = ppTypes

--------------------------------------------------

-- | 

ppTypes :: Types -> Doc i
ppTypes (Types cs) = PP.hcat (pretty <$> cs)

--------------------------------------------------

-- | @≡ 'runPrinter' 'ppTypes'@

prettyTypes :: Types -> String
prettyTypes = runPrinter ppTypes

-- '—'

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pTypes'@

instance Parse Types where

  parser = pTypes

--------------------------------------------------

pTypes :: (MTGParsing m) => m Types
pTypes = toTypes <$> p
  where

  p = many pColor

--------------------------------------------------

-- | @≡ 'pTypes'@

parseTypes :: (MonadThrow m) => String -> m Types
parseTypes = runParser 'Types pTypes 

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Types

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{-$setup

>>> :set -XPackageImports
>>> :set -XOverloadedStrings

-}

--------------------------------------------------
-- Elisp -----------------------------------------
--------------------------------------------------
{-

(defvar sboo-mtg-color-list '(white blue black red green) "All MTG types.")

(cl-defun sboo-mtg-read-color (&key prompt)
  (interactive)
  (let ((PROMPT        (or prompt "Color: "))
        (COLLECTION    sboo-mtg-color-list)
        (REQUIRE-MATCH t)
        )
    (completing-read PROMPT COLLECTION nil REQUIRE-MATCH)))

(defvar sboo-mtg-guild-list '(azorius dimir rakdos gruul selesnya orzhov izzet golgari boros simic) "All MTG guilds i.e. (color pairs).")

(cl-defun sboo-mtg-read-guild (&key prompt)
  (interactive)
  (let ((PROMPT        (or prompt "Guild: "))
        (COLLECTION    sboo-mtg-guild-list)
        (REQUIRE-MATCH t)
        )
    (completing-read PROMPT COLLECTION nil REQUIRE-MATCH)))

(defun sboo-mtg-haskell-insert-guild (name color1 color2)
  (interactive (list (sboo-mtg-read-guild :prompt "Guild: ") (sboo-mtg-read-color :prompt "1st Color: ") (sboo-mtg-read-color :prompt "2nd Color: ")))
  (require 's)
  (let* ((NAME   (s-capitalize name))
         (COLOR1 (s-capitalize color1))
         (COLOR2 (s-capitalize color2))

         (TEXT (format "\n-- | @≡ ['%s', '%s']@\npattern %s :: Types\npattern %s = Types [%s, %s]\n" COLOR1 COLOR2 NAME NAME COLOR1 COLOR2))
        )
    (insert TEXT)))

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------