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

module MTG.Enum.Symbol where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Enum.Color
import MTG.Enum.ManaSymbol

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Control.Lens (makePrisms)

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

newtype Symbol = Symbol Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseSymbol'@

instance IsString Symbol where

  fromString = (coerce . fromString)

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

toSymbol :: [ManaSymbol] -> Symbol
toSymbol symbols = cost
  where

  cost = Symbol symbols --TODO

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
prettySymbol Symbol = PP.braces docSymbol
  where

  docSymbol    = PP.pretty stringSymbol
  stringSymbol = (abbreviateSymbol Symbol) & fromMaybe ""

--------------------------------------------------

printSymbol :: Symbol -> String
printSymbol = (PP.String.renderString . prettySymbol)

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

instance Parse Symbol where

  parser = pSymbol

--------------------------------------------------

pSymbol :: Parser Symbol
pSymbol = p
  where

  p = _

--------------------------------------------------

parseSymbol :: (MonadThrow m) => String -> m Symbol
parseSymbol = runParser pSymbol 

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Symbol

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------