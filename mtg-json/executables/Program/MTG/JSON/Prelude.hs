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
  , module Program.MTG.JSON.Prelude
  , module MTG.JSON
  , module Prelude.Spiros
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "mtg-json"  MTG.JSON
import "mtg-types" MTG.Types

--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------

import "attoparsec" Data.Attoparsec.Text as EXPORT ( Parser(..) )

--------------------------------------------------

import Control.Exception as EXPORT ( ErrorCall(..) )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative             as P
import qualified "optparse-applicative" Options.Applicative.Help.Pretty as PP

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ( (%) )

--------------------------------------------------

import qualified "containers" Data.Map as Map

--------------------------------------------------

import qualified "base" Control.Arrow as Arrow

import qualified GHC.Exception as GHC ( errorCallWithCallStackException )

--------------------------------------------------
-- Definitions -----------------------------------
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

{- | Like `Map.fromList`, but preserving all values.

== Examples

>>> allFromList []
Map.fromList []
>>> allFromList [(1, 'a'), (2, 'b'), (1, 'c')]
Map.fromList [(1,"ab"), (2,"b")]

-}

allFromList :: (Ord k) => [(k,a)] -> Map k [a]
allFromList = map (Arrow.second (:[])) > Map.fromListWith (++)

--------------------------------------------------

{-| Parse an @Enum@, given an /association list/.

== Examples

>>> tBools = [ "off"-: False, "0"-: False, "on"-: True, "1"-: True ]
>>> rBool = pAssoc tBools
>>> import qualified Options.Applicative as P
>>> readBool t = P.getParseResult (P.execParserPure P.defaultPrefs (P.info mempty (P. rBool)) [t])
>>> readBool "on"
Just True
>>> readBool "1"
Just True
>>> readBool "???"
Nothing
>>> :t rBool
P.ReadM Bool

-}

pAssoc
  :: forall a.
    ( 
    )
  => [(String, a)]
  -> P.ReadM a

pAssoc kvs = rV

  where

  -- reader:

  rV :: P.ReadM a
  rV = P.maybeReader pV

  -- parser:

  pV :: String -> Maybe a
  pV s = Map.lookup s kvs' >>= listToMaybe

  -- cache:

  kvs' :: Map String [a]
  kvs' = allFromList kvs

{-# INLINEABLE pAssoc #-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------