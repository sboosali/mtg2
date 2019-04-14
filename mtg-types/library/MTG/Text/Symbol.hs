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

>>> pretty tapSymbol
{T}

-}

module MTG.Text.Symbol where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Text.Color
import MTG.Text.ManaSymbol

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

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

toSymbol :: ManaSymbol -> Symbol
toSymbol (ManaSymbol symbol) = (Symbol symbol)

--------------------------------------------------

loyaltyActivationSymbol :: Integer -> Symbol
loyaltyActivationSymbol i =
  if i >= 0
  then Symbol $ "+" <> s -- {+0}, {+1}, ...
  else Symbol $ "-" <> s -- {-1}, ...
  where
  s = (fromString . show) (abs i)

{-
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
  stringSymbol = case symbol of Symbol t -> t

--stringSymbol = (abbreviateSymbol symbol) & fromMaybe ""

--------------------------------------------------

printSymbol :: Symbol -> String
printSymbol = (PP.String.renderString . prettySymbol)

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

instance Parse Symbol where

  parser = pSymbol

--------------------------------------------------

pSymbol :: MTGParsing m => m Symbol
pSymbol = p
  where

  p = _

--------------------------------------------------

parseSymbol :: (MonadThrow m) => String -> m Symbol
parseSymbol = runParser 'Symbol pSymbol 
-}

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Symbol

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------