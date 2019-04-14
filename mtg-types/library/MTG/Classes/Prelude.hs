--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Custom @Prelude@, for the @MTG.Classes.*@ modules.

Which themselves are re-exported by "MTG.Types.Prelude"
(the package-specific custom @Prelude@).

-}

module MTG.Classes.Prelude

  ( module MTG.Classes.Prelude
  , module EXPORT
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros as EXPORT

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc               as PP
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

import           "prettyprinter" Data.Text.Prettyprint.Doc ( Doc )

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

-- | Association List.

type Assoc a = [( Text, a )]

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | Run an 'MTGPrinting' pretty-printer.

== Implementation

Wraps `PP.String.renderString` and `PP.layoutSmart`.

-}

runPrinter
  :: (a -> Doc i)
  -> (a -> String)

runPrinter f = f > layout > render
  where

  layout = PP.layoutSmart PP.defaultLayoutOptions
  render = PP.String.renderString
  
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------