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

{-| 

== Examples

Construction:

e.g. 'toColors':

>>> toColors [ Blue, Green, Green, Blue ]
Colors ["Green","Blue"]
>>> toColors ["Green","Blue"] == toColors ["Blue","Green"]
True

Printing (see 'pretty'):

e.g. 'prettyColors':

>>> pretty Simic
GU

Parsing (see 'parser'):

e.g. 'parseColors':

>>> parseColors "UG"
Colors [Color "Green",Color "Blue"]

-}

module MTG.Text.List.Colors

  ( module MTG.Text.List.Colors
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import qualified MTG.Enum.Color as Color
import           MTG.Text.Color

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

* 'toColors'

-}

newtype Colors = Colors

  [Color]

  deriving stock    (Show,Read)
  deriving stock    (Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'toColors'@

instance IsList Colors where
  type Item Colors = Color
  fromList = toColors
  toList   = coerce

--------------------------------------------------

-- | @≡ 'parseColors'@

instance IsString Colors where
  fromString = fromString_MonadThrow parseColors

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------
-- Guilds (ally-colors)...

-- | @≡ ['White', 'Blue']@
pattern Azorius :: Colors
pattern Azorius = Colors [White, Blue]

-- | @≡ ['Blue', 'Black']@
pattern Dimir :: Colors
pattern Dimir = Colors [Blue, Black]

-- | @≡ ['Black', 'Red']@
pattern Rakdos :: Colors
pattern Rakdos = Colors [Black, Red]

-- | @≡ ['Red', 'Green']@
pattern Gruul :: Colors
pattern Gruul = Colors [Red, Green]

-- | @≡ ['Green', 'White']@
pattern Selesnya :: Colors
pattern Selesnya = Colors [Green, White]

--------------------------------------------------
-- Guilds (enemy-color)...

-- | @≡ ['White', 'Black']@
pattern Orzhov :: Colors
pattern Orzhov = Colors [White, Black]

-- | @≡ ['Blue', 'Red']@
pattern Izzet :: Colors
pattern Izzet = Colors [Blue, Red]

-- | @≡ ['Black', 'Green']@
pattern Golgari :: Colors
pattern Golgari = Colors [Black, Green]

-- | @≡ ['Red', 'White']@
pattern Boros :: Colors
pattern Boros = Colors [Red, White]

-- | @≡ ['Green', 'Blue']@
pattern Simic :: Colors
pattern Simic = Colors [Green, Blue]

--------------------------------------------------
-- Shards...

--------------------------------------------------
-- Wedges...

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

allGuilds :: [Colors]
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
knownTrichromes :: Colors
knownTrichromes =

  [ 
  , 
  ]

--------------------------------------------------

knownTetrachromes :: Colors
knownTetrachromes =

  [ 
  , 
  ]
-}

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Accessor for 'Colors'.

getColors :: Colors -> [Color]
getColors (Colors cs) = cs

--------------------------------------------------

-- | Smart Constructor for 'Colors'.

toColors :: [Color] -> Colors
toColors unsortedColors = Colors sortedColors
  where

  sortedColors = sortColors uniqueColors
  uniqueColors = ordNub unsortedColors

--------------------------------------------------

{- | Wraps `Color.sortMTGColors`.

Outputs:

* an /idiomatically sorted/ prefix of known colors.
* an “unsorted” (i.e. sorted by 'Ord') suffix of unknown colors.

-}

sortColors :: [Color] -> [Color]
sortColors

  = fmap isColorKnown
  > partitionEithers
  > go

  where

  go ( unknownColors, knownColors )
    = sortKnownColors knownColors
   ++ sortUnknownColors unknownColors

  sortKnownColors :: [Color.Color] -> [Color]
  sortKnownColors
    = Color.sortMTGColors
    > fmap (show > Text.pack > Color)

  sortUnknownColors :: [Text] -> [Color] 
  sortUnknownColors
    = sort
    > fmap Color

{-# INLINEABLE sortColors #-}

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

-- | @≡ 'ppColors'@

instance Pretty Colors where

  pretty = ppColors

--------------------------------------------------

-- | 

ppColors :: Colors -> Doc i
ppColors (Colors cs) = PP.hcat (pretty <$> cs)

--------------------------------------------------

-- | @≡ 'runPrinter' 'ppColors'@

prettyColors :: Colors -> String
prettyColors = runPrinter ppColors

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pColors'@

instance Parse Colors where

  parser = pColors

--------------------------------------------------

pColors :: (MTGParsing m) => m Colors
pColors = toColors <$> p
  where

  p = many pColor

--------------------------------------------------

-- | @≡ 'pColors'@

parseColors :: (MonadThrow m) => String -> m Colors
parseColors = runParser 'Colors pColors 

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Colors

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

(defvar sboo-mtg-color-list '(white blue black red green) "All MTG colors.")

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

         (TEXT (format "\n-- | @≡ ['%s', '%s']@\npattern %s :: Colors\npattern %s = Colors [%s, %s]\n" COLOR1 COLOR2 NAME NAME COLOR1 COLOR2))
        )
    (insert TEXT)))

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------