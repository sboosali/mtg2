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

>>> pretty (Symbol "U")
{U}
>>> pretty tapSymbol
{T}

-}

module MTG.Text.Symbol where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--import MTG.Text.Color
import MTG.Text.ManaSymbol

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

newtype Symbol = Symbol

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseSymbol'@

instance IsString Symbol where

  fromString = (Symbol . fromString)

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

tapSymbol :: Symbol
tapSymbol = "T"

untapSymbol :: Symbol
untapSymbol = "Q"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | Accessor for 'Symbol'.

getSymbolText :: Symbol -> Text
getSymbolText (Symbol t) = t

--------------------------------------------------

-- | Each 'ManaSymbol' is a 'Symbol'.

fromManaSymbol :: ManaSymbol -> Symbol
fromManaSymbol (ManaSymbol t) = (Symbol t)

--------------------------------------------------

loyaltyActivationSymbol :: Integer -> Symbol
loyaltyActivationSymbol i =
  if i >= 0
  then Symbol $ "+" <> s -- {+0}, {+1}, ...
  else Symbol $ "-" <> s -- {-1}, ...
  where
  s = (fromString . show) (abs i)

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'prettySymbol'@

instance Pretty Symbol where

  pretty = prettySymbol

--------------------------------------------------

prettySymbol :: Symbol -> Doc i
prettySymbol symbol = PP.braces docSymbol
  where

  docSymbol    = PP.pretty stringSymbol
  stringSymbol = getSymbolText symbol

--stringSymbol = (abbreviateSymbol symbol) & fromMaybe ""

--------------------------------------------------

printSymbol :: Symbol -> String
printSymbol = runPrinter prettySymbol

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pSymbol'@

instance Parse Symbol where

  parser = pSymbol

--------------------------------------------------

pSymbol :: MTGParsing m => m Symbol
pSymbol = Symbol <$> pSymbolText <?> "Symbol"

--------------------------------------------------

-- | @≡ 'pSymbol'@

parseSymbol :: (MonadThrow m) => String -> m Symbol
parseSymbol = runParser 'Symbol pSymbol 

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Symbol

--------------------------------------------------
-- Doctest ---------------------------------------
--------------------------------------------------

{-$setup

>>> :set -XOverloadedStrings

-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------