--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{-# LANGUAGE ImplicitParams #-}

--------------------------------------------------

{- | Re-export:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).
* the "MTG.Types" @module@ — re-exports most of the @mtg-types@ package.
* the "MTG.JSON" @module@ — tre-exports most of the @mtg-json@ package (the @library@).

-}

module Program.MTG.JSON.Prelude

  ( module EXPORT

  , module MTG.JSON
  , module MTG.Types

  , module Program.MTG.JSON.Prelude
  , module Prelude.Spiros
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "mtg-json"  MTG.JSON
import "mtg-types" MTG.Types hiding ( pAssoc )

--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------

import "base" Control.Exception as EXPORT ( ErrorCall(..) )
import "base" System.Exit       as EXPORT ( ExitCode(..) )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative             as P

import qualified "optparse-applicative" Options.Applicative.Help.Pretty as PP

--------------------------------------------------

import qualified "formatting" Formatting as Format

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map

--------------------------------------------------

import qualified "base" Control.Arrow as Arrow

import qualified "base" System.IO as IO

import qualified "base" GHC.Exception as GHC ( errorCallWithCallStackException )

import qualified "base" Prelude

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

putStdErr :: String -> IO ()
putStdErr = IO.hPutStr IO.stderr

{-# INLINEABLE putStdErr #-}

--------------------------------------------------

putStdOut :: String -> IO ()
putStdOut = IO.hPutStr IO.stdout

{-# INLINEABLE putStdOut #-}

--------------------------------------------------

printDivider :: IO ()
printDivider = do

  putStr "\n----------------------------------------\n"

{-# INLINEABLE printDivider #-}

--------------------------------------------------

errorCall :: (HasCallStack) => String -> SomeException
errorCall s = GHC.errorCallWithCallStackException s ?callStack

-- errorCallWithCallStackException :: String -> CallStack -> SomeException

{-# INLINEABLE errorCall #-}

--------------------------------------------------

putAnsiDocs :: (Traversable f) => f PP.Doc -> IO ()
putAnsiDocs docs = PP.putDoc `traverse_` docs

{-# INLINEABLE putAnsiDocs #-}

{-# SPECIALIZE putAnsiDocs :: [PP.Doc]       -> IO () #-}
{-# SPECIALIZE putAnsiDocs :: Maybe (PP.Doc) -> IO () #-}
{-# SPECIALIZE putAnsiDocs :: Either e (PP.Doc) -> IO () #-}

--hPutDoc :: Handle -> Doc -> IO ()

--------------------------------------------------

{- | Run a format-specification.

== Implementation

Wraps `Format.formatToString`

-}

runFormat :: Format.Format String a -> a
runFormat = Format.formatToString

{-# INLINEABLE runFormat #-}

--------------------------------------------------

{- |

== Exports

>>> fromExitCode ExitSuccess
0
>>> fromExitCode (ExitFailure 1)
1

-}

fromExitCode :: ExitCode -> Int
fromExitCode = \case

  ExitSuccess   -> 0
  ExitFailure i -> i

--------------------------------------------------

{- | Like `Map.fromList`, but preserving all values.

== Examples

>>> allFromList []
fromList []
>>> allFromList [(1, 'a'), (2, 'b'), (1, 'c')]
fromList [(1,"ca"),(2,"b")]

-}

allFromList :: (Ord k) => [(k,a)] -> Map k [a]
allFromList = map (Arrow.second (:[])) > Map.fromListWith (++)

--------------------------------------------------

{-| Create a /parser/ and a /completer/ for an “@Enum@-@String@”, given an /association list/.

== Examples

>>> tBools = [ "off"-: False, "0"-: False, "on"-: True, "1"-: True ]
>>> ( rBool, _ ) = pAssoc tBools
>>> import qualified "optparse-applicative" Options.Applicative as P
>>> import qualified "base" Prelude
>>> readBool t = (P.getParseResult (P.execParserPure P.defaultPrefs (P.info (P.argument rBool Prelude.mempty) Prelude.mempty) [t]))
>>> readBool "on"
Just True
>>> readBool "1"
Just True
>>> readBool "???"
Nothing
>>> :t rBool
rBool :: P.ReadM Bool

-}

pAssoc
  :: forall a.
    ( 
    )
  => [(String, a)]
  -> ( P.ReadM a, [String] )

pAssoc kvs = ( rV, cV )

  where

  -- reader:

  rV :: P.ReadM a
  rV = P.maybeReader pV

  -- completions:

  cV :: [String]
  cV = fst <$> kvs

  -- parser:

  pV :: String -> Maybe a
  pV s = Map.lookup s kvs' >>= listToMaybe

  -- cache:

  kvs' :: Map String [a]
  kvs' = allFromList kvs

{-# INLINEABLE pAssoc #-}

--------------------------------------------------
-- Examples --------------------------------------
--------------------------------------------------

-- | (type-checked 'pAssoc' example).
example_pAssoc :: String -> Maybe Bool
example_pAssoc t = (P.getParseResult (P.execParserPure P.defaultPrefs (P.info (P.argument rBool Prelude.mempty) Prelude.mempty) [t]))
  where

  tBools = [ "off"-: False, "0"-: False, "on"-: True, "1"-: True ]
  ( rBool, _ ) = pAssoc tBools

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------