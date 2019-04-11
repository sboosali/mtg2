--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

--------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-| 

== Examples

>>> pretty (toManaCost [ toManaSymbol "2", toManaSymbol "u", toManaSymbol "g" ])
{2}{U}{G}

-}


module MTG.List.ManaCost

  ( module MTG.List.ManaCost

  , module MTG.Enum.ManaSymbol
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Enum.ManaSymbol
import MTG.List.Colors

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "attoparsec" Data.Attoparsec.Text as P

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|

-}

newtype ManaCost = ManaCost

  [ManaSymbol]

  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'toManaCost'@

instance IsList ManaCost where

  type Item ManaCost = ManaSymbol

  fromList = toManaCost
  toList   = coerce

--------------------------------------------------

-- | @≡ 'parseManaCost'@

instance IsString ManaCost where

  fromString = (coerce . fromString)

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

noManaCost :: ManaCost
noManaCost = []

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

toManaCost :: [ManaSymbol] -> ManaCost
toManaCost symbols = cost
  where

  cost = ManaCost symbols --TODO

--------------------------------------------------

colorsToManaCost :: Colors -> ManaCost
colorsToManaCost = _

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppManaCost'@

instance Pretty ManaCost where

  pretty = ppManaCost

--------------------------------------------------

ppManaCost :: ManaCost -> PP.Doc i
ppManaCost ManaCost = PP.braces docManaCost
  where

  docManaCost    = PP.pretty stringManaCost
  stringManaCost = (abbreviateManaCost ManaCost) & fromMaybe ""

--------------------------------------------------

-- | @≡ 'ppManaCost'@

prettyManaCost :: ManaCost -> String
prettyManaCost = (PP.String.renderString . ppManaCost)

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pManaCost'@

instance Parse ManaCost where

  parser = pManaCost

--------------------------------------------------

pManaCost :: (MTGParsing m) => m ManaCost
pManaCost = _

--------------------------------------------------

-- | @≡ 'pManaCost'@

parseManaCost :: (MonadThrow m) => String -> m ManaCost
parseManaCost = runParser pManaCost 

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''ManaCost

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------