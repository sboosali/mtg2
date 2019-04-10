{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| 

== Examples

e.g. 'Simic':

>>> pretty Simic
{G}{U}

-}

module MTG.Colors

  ( Colors

  , module MTG.Enum.Color
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Enum.Color

import MTG.Types.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
--import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "text" Data.Text as T

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| Set of 'Color's.

== Constructors

* 'sortColors'

-}

newtype Colors = Colors

  [Color]

  deriving stock    (Show,Read)
  deriving stock    (Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'sortColors'@

instance IsList Colors where
  type Item Colors = Color
  fromList = sortColors
  toList   = coerce

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

sortColors :: [Color] -> Colors
sortColors unsortedColors = Colors sortedColors
  where

  sortedColors = sort unsortedColors

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
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'prettyColor'@

instance Pretty Colors where

  pretty = prettyColors

--------------------------------------------------

prettyColors :: Colors -> PP.Doc i
prettyColors (Colors cs) = PP.hcat (pretty <$> cs)

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