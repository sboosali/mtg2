--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{- | Sources and Destinations.

-}

module Data.SrcDst

  (

  -- * Types

    SrcDst(..)
  , Src(..)
  , Dst(..)

  , DstSrcs(..)
  , RemoteSrc(..)
  , RemoteSrcs(..)
  , LocalSrc(..)

  , URL(..)

  -- * Introducers

  , toDstSrcs
  , toDstSrcsM

  , parseSrc
  , parseDst
  , parseLocalSrc
  , parseRemoteSrc

  -- * Transformers

  , partitionSrcs
  , fromSrc

  , fromLocalSrc
  , fromRemoteSrc 

  -- * Eliminators

  , fromDstSrcs

  , prettySrc
  , prettyDst

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Prelude.SrcDst 

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
import qualified "containers" Data.Set as Set

--------------------------------------------------

import qualified "bytestring" Data.ByteString.Lazy             as Lazy
import qualified "bytestring" Data.ByteString.Char8            as StrictASCII
import qualified "bytestring" Data.ByteString.Lazy.Char8       as LazyASCII

--------------------------------------------------

import qualified "base" Data.List as List

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | Read the source ('Src'), and write it to a destination ('Dst').

-}

data SrcDst = SrcDst

  { src :: Src
  , dst :: Dst
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{- | A local or remote source of some data.

-}

data Src

  = SrcBytes  LazyBytes
  | SrcBytes' StrictBytes

  | SrcStdin
  | SrcUri   URL
  | SrcFile  FilePath

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseSrc'@
instance IsString Src where fromString = parseSrc

--------------------------------------------------
--------------------------------------------------

{- | A local destination for some data.

-}

data Dst

  = DstStdout
  | DstFile    FilePath

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseDst'@
instance IsString Dst where fromString = parseDst

--------------------------------------------------
--------------------------------------------------

{- | Multiple `SrcDst`s.

Each destination has (exactly) one source; i.e.
sources shouldn't collide.

== Implementation

`SrcDst` is represented “reversed”, into @( `Dst`, `Src` )@.
This enforces the "unique destination" property.

-}

newtype DstSrcs = DstSrcs

  ( Map Dst Src )

  deriving stock    (Lift,Generic)
  deriving stock    (Show,Read)    -- print&parse with constructor.
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData)

--------------------------------------------------

-- | @`fromList` ≡ `toDstSrcs`@
instance IsList DstSrcs where
  type Item DstSrcs = SrcDst
  fromList = toDstSrcs
  toList   = fromDstSrcs

--------------------------------------------------
--------------------------------------------------

{- | A set of remote sources.

-}

newtype RemoteSrcs = RemoteSrcs

  (Set RemoteSrc)

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving newtype  (Eq, Ord)

  deriving newtype  (Semigroup, Monoid)
  deriving newtype  (NFData{-, Hashable-})

--------------------------------------------------

instance IsList RemoteSrcs where

  type Item RemoteSrcs = RemoteSrc

  fromList = Set.fromList > coerce
  toList   = coerce > Set.toList

--------------------------------------------------
--------------------------------------------------

{- | A “remote” data source.

-}

data RemoteSrc

  = RemoteSrcUri URL

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseRemoteSrc'@
instance IsString RemoteSrc where fromString = parseRemoteSrc

--------------------------------------------------
--------------------------------------------------

{- | A “local” data source.

-}

data LocalSrc

  = LocalSrcBytes LazyBytes
  | LocalSrcStdin
  | LocalSrcFile FilePath

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseLocalSrc'@
instance IsString LocalSrc where fromString = parseLocalSrc

--------------------------------------------------
--------------------------------------------------

{- | 

-}

newtype URL = URL

  { fromURL ::
      String
  }

  deriving stock    (Lift,Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Show,Read) -- NOTE -- hides accessor from printing.
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString URL where
  fromString = coerce

--------------------------------------------------
-- Functions: Conversion -------------------------
--------------------------------------------------

{- | Create a `DstSrcs` from multiple `Dst`s and `Src`s.

Identical `SrcDst`s are redundant.

== Examples

>>> :set -XOverloadedLists
>>> DstSrcs kvs = [ SrcDst{ src = SrcUri (URL "https://mtgjson.com/json/Vintage.json.gz"), dst = DstFile "vintage.json" }, SrcDst{ dst = DstStdout, src = SrcStdin }, SrcDst{ src = SrcUri (URL "https://mtgjson.com/json/Vintage.json.gz"), dst = DstFile "mtg.json" } ]
>>> destinations = Map.keys  kvs
>>> sources      = Map.elems kvs
>>> length (nub destinations)
3
>>> length (nub sources)
2
>>> DstSrcs kvs
DstSrcs (fromList [(DstStdout,SrcStdin),(DstFile "mtg.json",SrcUri "https://mtgjson.com/json/Vintage.json.gz"),(DstFile "vintage.json",SrcUri "https://mtgjson.com/json/Vintage.json.gz")])

-}

toDstSrcs :: [SrcDst] -> DstSrcs
toDstSrcs srcdsts = DstSrcs kvs
  where

  kvs :: Map Dst Src
  kvs
    = attributes
    & Map.fromList

  attributes :: [( Dst, Src )]
  attributes = fromSrcDst <$> srcdsts

  fromSrcDst SrcDst{ src, dst } = ( dst, src )

--------------------------------------------------

{- | Expand `DstSrcs` into each individual `SrcDst`.

== Examples

>>> fromDstSrcs (DstSrcs (Map.fromList [( DstFile "vintage.json", SrcUri (URL "https://mtgjson.com/json/Vintage.json.gz") ), ( DstStdout, SrcStdin ), ( DstFile "mtg.json", SrcUri (URL "https://mtgjson.com/json/Vintage.json.gz") )]))
[SrcDst {src = SrcStdin, dst = DstStdout},SrcDst {src = SrcUri "https://mtgjson.com/json/Vintage.json.gz", dst = DstFile "mtg.json"},SrcDst {src = SrcUri "https://mtgjson.com/json/Vintage.json.gz", dst = DstFile "vintage.json"}]

-}

fromDstSrcs :: DstSrcs -> [SrcDst]
fromDstSrcs (DstSrcs kvs) = vks
  where

  vks
    = kvs
    & Map.toList
    & fmap toSrcDst

  toSrcDst ( dst, src ) = SrcDst{ src, dst }

--------------------------------------------------

{- | Create a `DstSrcs` from multiple `Dst`s and `Src`s.

`toDstSrcs` vs `toDstSrcsM`:

* `toDstSrcs` ignores collisions.
* `toDstSrcsM` fails on any collision (reporting all collisions, not just the first).

== Examples

>>> toDstSrcsM []
DstSrcs (fromList [])

-}

toDstSrcsM :: ( MonadThrow m ) => [SrcDst] -> m DstSrcs
toDstSrcsM srcdsts = do

  if   noCollision
  then return dstsrcs
  else errorM e

  where

  dstsrcs :: DstSrcs
  dstsrcs = toDstSrcs srcdsts

  noCollision :: Bool
  noCollision = allSingletons groupedDstsBySrc

  groupedDstsBySrc :: [[SrcDst]]
  groupedDstsBySrc
    = srcdsts
    & List.groupBy (\SrcDst{ dst = x } SrcDst{ dst = y } -> x == y)

  -- group `Dst`s by their `Src`, then check for only singletons.

  e :: String
  e = "Colliding destinations"  -- TODO format.

  allSingletons :: (Eq a) => [[a]] -> Bool
  allSingletons = List.all (\xs -> length xs <= 1)

--------------------------------------------------

{- | Distinguish `LocalSrc`s from `RemoteSrc`s. -}

partitionSrcs :: [Src] -> ( [RemoteSrc], [LocalSrc] )
partitionSrcs sources = ( remotes, locals )
  where

  ( remotes, locals ) = partitionEithers sources'

  sources' = fromSrc <$> sources

--------------------------------------------------

{- | A `Src` is either *local* or *remote*. -}

fromSrc :: Src -> Either RemoteSrc LocalSrc
fromSrc = \case

  SrcBytes  bs  -> Right (LocalSrcBytes bs)
  SrcBytes' bs' -> Right (LocalSrcBytes (Lazy.fromChunks [bs']))
  SrcStdin      -> Right LocalSrcStdin
  SrcFile   fp  -> Right (LocalSrcFile fp)

  SrcUri url -> Left (RemoteSrcUri url)

--------------------------------------------------

{- | Generalize a `LocalSrc`. -}

fromLocalSrc :: LocalSrc -> Src
fromLocalSrc = \case

  LocalSrcBytes bs  -> SrcBytes bs
  LocalSrcStdin      -> SrcStdin
  LocalSrcFile  fp  -> SrcFile fp

--------------------------------------------------

{- | Generalize a `RemoteSrc`. -}

fromRemoteSrc :: RemoteSrc -> Src
fromRemoteSrc = \case

  RemoteSrcUri url -> SrcUri url

--------------------------------------------------
-- Functions: Printing / Parsing -----------------
--------------------------------------------------

{- | 
== Examples

>>> parseSrc "-"
SrcStdin
>>> parseSrc "./mtg.json"
SrcFile "./mtg.json"
>>> parseSrc "          ./mtg.json          "
SrcFile "./mtg.json"

-}

parseSrc :: String -> Src
parseSrc = munge > \case

  "-" -> SrcStdin

  s -> SrcFile s

  where

  munge = lrstrip

--------------------------------------------------

{- | 
== Examples

>>> parseDst "-"
DstStdout
>>> parseDst "./mtg.hs"
DstFile "./mtg.hs"
>>> parseDst "          ./mtg.hs          "
DstFile "./mtg.hs"

-}

parseDst :: String -> Dst
parseDst = munge > \case

  "-" -> DstStdout

  s -> DstFile s

  where

  munge = lrstrip

--------------------------------------------------

{- | 
== Examples

>>> parseLocalSrc "-"
LocalSrcStdin
>>> parseLocalSrc "./mtg.json"
LocalSrcFile "./mtg.json"
>>> parseLocalSrc "          ./mtg.json          "
LocalSrcFile "./mtg.json"

-}

parseLocalSrc :: String -> LocalSrc
parseLocalSrc = munge > \case

  "-" -> LocalSrcStdin

  s -> LocalSrcFile s

  where

  munge = lrstrip

--------------------------------------------------

{- | 
== Examples

-}

parseRemoteSrc :: String -> RemoteSrc
parseRemoteSrc = munge > fromString > RemoteSrcUri

  where

  munge = lrstrip

--------------------------------------------------

prettySrc :: Src -> String
prettySrc = \case

  SrcBytes  bs -> LazyASCII.unpack   bs
  SrcBytes' bs -> StrictASCII.unpack bs

  SrcStdin   -> "<<stdin>>"
  SrcFile fp -> "" <> fp
  SrcUri url -> "" <> fromURL url

--------------------------------------------------

prettyDst :: Dst -> String
prettyDst = \case

  DstStdout  -> "<<stdout>>"
  DstFile fp -> "" <> fp

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- 

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------