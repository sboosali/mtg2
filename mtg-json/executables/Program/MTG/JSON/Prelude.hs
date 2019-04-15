--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{-# LANGUAGE ImplicitParams #-}

--------------------------------------------------

{- | Re-export:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).
* the "MTG.Types.Prelude" @module@ — the @mtg-types@ package's custom prelude.
* the "MTG.JSON.Prelude" @module@ — the @mtg-json@ package's custom prelude (the library).

-}

module Program.MTG.JSON.Prelude

  ( module EXPORT
  , module Prelude.Spiros
  , module MTG.Types.Prelude
  , module MTG.JSON.Prelude
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "mtg-json"  MTG.JSON.Prelude
import "mtg-types" MTG.Types.Prelude

--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------

import "prettyprinter" Data.Text.Prettyprint.Doc as EXPORT ( Pretty(..) )

--------------------------------------------------

import "attoparsec" Data.Attoparsec.Text as EXPORT ( Parser(..) )

--------------------------------------------------

import Control.Exception as EXPORT ( ErrorCall(..) )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified GHC.Exception as GHC ( errorCallWithCallStackException )

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

printDivider :: IO ()
printDivider = do

  putStrLn "----------------------------------------"

--------------------------------------------------

errorCall :: (HasCallStack) => String -> SomeException
errorCall s = GHC.errorCallWithCallStackException s ?callStack

-- errorCallWithCallStackException :: String -> CallStack -> SomeException

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------