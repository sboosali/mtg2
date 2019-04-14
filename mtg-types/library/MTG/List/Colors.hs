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
Colors [Color "Green",Color "Blue"]

>>> toColors simic == toColors (reverse simic)
True

Printing (see 'pretty'):

e.g. 'prettyColors':

>>> pretty Simic
{G}{U}

Parsing (see 'parser'):

e.g. 'parseColors':

>>> parseColors "UG"
Colors [Color "Green",Color "Blue"]

-}

module MTG.List.Colors

  ( module MTG.List.Colors

  , module MTG.Text.Color
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Text.Color

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
--import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

--import qualified "attoparsec" Data.Attoparsec.Text as P

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--import qualified "text" Data.Text as T

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

toColors :: [Color] -> Colors
toColors unsortedColors = Colors sortedColors
  where

  sortedColors = sortColors uniqueColors --TODO
  uniqueColors = ordNub unsortedColors

sortColors :: [Color] -> [Color]
sortColors = id

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