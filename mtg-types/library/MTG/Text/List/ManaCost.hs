--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

--------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{- | 

== Examples

>>> pretty (toManaCost [ toManaSymbol "2", toManaSymbol "u", toManaSymbol "g" ])
{2}{U}{G}

-}

module MTG.Text.List.ManaCost

  ( module MTG.Text.List.ManaCost
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Text.ManaSymbol
import MTG.Text.List.Colors

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

-- import qualified "text" Data.Text as Text

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

  fromString = fromString_MonadThrow parseManaCost

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

noManaCost :: ManaCost
noManaCost = []

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Accessor for 'ManaCost'.

getManaCost :: ManaCost -> [ManaSymbol]
getManaCost (ManaCost ss) = ss

--------------------------------------------------

toManaCost :: [ManaSymbol] -> ManaCost
toManaCost symbols = cost
  where

  cost = ManaCost symbols --TODO

--------------------------------------------------

colorsToManaCost :: Colors -> ManaCost
colorsToManaCost

  = getColors
  > fmap colorToManaSymbol
  > ManaCost

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppManaCost'@

instance Pretty ManaCost where

  pretty = ppManaCost

--------------------------------------------------

ppManaCost :: ManaCost -> Doc i
ppManaCost (ManaCost ss) = PP.hcat (pretty <$> ss)

--------------------------------------------------

-- | @≡ 'runPrinter' 'ppManaCost'@

prettyManaCost :: ManaCost -> String
prettyManaCost = runPrinter ppManaCost

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pManaCost'@

instance Parse ManaCost where

  parser = pManaCost

--------------------------------------------------

pManaCost :: (MTGParsing m) => m ManaCost
pManaCost = toManaCost <$> p
  where

  p = many pManaSymbol

--------------------------------------------------

-- | @≡ 'pManaCost'@

parseManaCost :: (MonadThrow m) => String -> m ManaCost
parseManaCost = runParser 'ManaCost pManaCost

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''ManaCost

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{-$setup

>>> :set -XPackageImports
>>> :set -XOverloadedStrings

-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------