--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DuplicateRecordFields #-}

--------------------------------------------------

{- | Sources and Destinations.

-}

module Data.SrcDst

  (

  -- * Types

    SrcDst(..)
  , Src(..)
  , Dst(..)
  , RemoteSrc(..)
  , LocalSrc(..)

  , DstSrcs(..)
  , RemoteSrcs(..)
  , LocalSrcs(..)

  , URL(..)

  -- * Introducers

  , toDstSrcsM
  , toDstSrcs

  , parseSrc
  , parseDst
  , parseLocalSrc
  , parseRemoteSrc

  , toUrl

  -- * Transformers

  , partitionSrcs
  , unifySrcs

  , fromSrc
  , fromLocalSrc
  , fromRemoteSrc 

  -- * Eliminators

  , fromDstSrcs

  , prettySrc
  , prettyDst

  -- * Validation

  , CheckDstSrcs(..)
  , CheckCollisions(..)
  , CheckHandles(..)

  , defaultCheckDstSrcs
  , lenientCheckDstSrcs
  , stringentCheckDstSrcs 

  , toDstSrcsWithM
  , toDstSrcsLenientlyM
  , toDstSrcsStringentlyM

  , isUrlHttps

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

import qualified "text" Data.Text as Text

--------------------------------------------------

import qualified "bytestring" Data.ByteString.Lazy             as Lazy
import qualified "bytestring" Data.ByteString.Char8            as StrictASCII
import qualified "bytestring" Data.ByteString.Lazy.Char8       as LazyASCII

--------------------------------------------------

import qualified "base" Data.Char as Char
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

{- | A destination for some data.

/NOTE/ ALl `Dst`s are “local”, thus there is no @LocalDst@
(as there is a `LocalSrc`).

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

{- | A set of /remote/ sources.

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

{- | A set of /local/ sources.

-}

newtype LocalSrcs = LocalSrcs

  (Set LocalSrc)

  deriving stock    (Generic)
  deriving stock    (Show, Read)
  deriving newtype  (Eq, Ord)

  deriving newtype  (Semigroup, Monoid)
  deriving newtype  (NFData{-, Hashable-})

--------------------------------------------------

instance IsList LocalSrcs where

  type Item LocalSrcs = LocalSrc

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

{- | @URL@ is a “/Uniform Resource Locator/”.

== Construct

* `toUrl`
* `fromString`

-}

newtype URL = URL

  { fromURL ::
      Text
  }

  deriving stock    (Lift,Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Show,Read) -- NOTE -- hides accessor from printing.
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @≡ `toUrl`@

instance IsString URL where fromString = toUrl

--------------------------------------------------
--------------------------------------------------

{- | Validation of `SrcDst`s (by `toDstSrcsM`).

== Values

Include:

* `defaultCheckDstSrcs`
* `lenientCheckDstSrcs`
* `stringentCheckDstSrcs`

== Uses

`toDstSrcs` (and thus the `IsList` instance of `SrcDst`) behaves like
`lenientCheckDstSrcs`, which is /maximally lenient/.

`toDstSrcsM` uses `defaultCheckDstSrcs`, which is /stringent/
(but not /maximally stringent/).

-}

data CheckDstSrcs = CheckDstSrcs

  { dstCollision :: CheckCollisions
  , dstUniqueness :: CheckHandles
  , srcSecurity  :: CheckURLs
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Data,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'defaultCheckDstSrcs'@

instance Default CheckDstSrcs where def = defaultCheckDstSrcs

--------------------------------------------------

{- | Prohibits multiple (different) sources from writing to the same destination.

Fields:

* @`dstCollision` = `ProhibitCollisions`@
* @`dstUniqueness` = `RequireUniqueHandles`@
* @`srcSecurity`  = `AllowHTTP`@

-}

defaultCheckDstSrcs :: CheckDstSrcs
defaultCheckDstSrcs = CheckDstSrcs{..}
  where

  dstCollision = ProhibitCollisions
  dstUniqueness = RequireUniqueHandles
  srcSecurity  = AllowHTTP

--------------------------------------------------

{- | Check leniently. -}

lenientCheckDstSrcs :: CheckDstSrcs
lenientCheckDstSrcs = CheckDstSrcs{..}
  where

  dstCollision = IgnoreCollisions
  dstUniqueness = IgnoreDuplicateHandles
  srcSecurity  = AllowHTTP

--------------------------------------------------

{- | Check stringently. -}

stringentCheckDstSrcs :: CheckDstSrcs
stringentCheckDstSrcs = CheckDstSrcs{..}
  where

  dstCollision = ProhibitCollisions
  dstUniqueness = RequireUniqueHandles
  srcSecurity  = RequireHTTPS

--------------------------------------------------
--------------------------------------------------

{- | 

-}

data CheckCollisions

  = IgnoreCollisions
  | ProhibitCollisions

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Data,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'defaultCheckCollisions'@

instance Default CheckCollisions where def = defaultCheckCollisions

--------------------------------------------------

-- | @= 'IgnoreCollisions'@

defaultCheckCollisions :: CheckCollisions
defaultCheckCollisions = IgnoreCollisions

--------------------------------------------------
--------------------------------------------------

{- | 

-}

data CheckHandles

  = IgnoreDuplicateHandles
  | AllowDuplicateHandles
  | RequireUniqueHandles

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Data,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'defaultCheckHandles'@

instance Default CheckHandles where def = defaultCheckHandles

--------------------------------------------------

-- | @= 'AllowDuplicateHandles'@

defaultCheckHandles :: CheckHandles
defaultCheckHandles = AllowDuplicateHandles

--------------------------------------------------

{- | 

-}

data CheckURLs

  = AllowHTTP
  | RequireHTTPS

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Data,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{- | @≡ 'defaultCheckURLs'@ -}

instance Default CheckURLs where def = defaultCheckURLs

--------------------------------------------------

{- | @≡ 'AllowHTTP'@ -}

defaultCheckURLs :: CheckURLs
defaultCheckURLs = AllowHTTP

--------------------------------------------------
-- Functions: Conversion -------------------------
--------------------------------------------------

{- | Create a `DstSrcs` from multiple `Dst`s and `Src`s.

Identical `SrcDst`s are redundant.

== Examples

>>> :set -XOverloadedLists
>>> :set -XOverloadedStrings
>>> DstSrcs kvs = [ SrcDst{ src = SrcUri (URL "https://mtgjson.com/json/Vintage.json.gz"), dst = DstFile "vintage.json" }, SrcDst{ dst = DstStdout, src = SrcStdin }, SrcDst{ src = SrcUri (URL "https://mtgjson.com/json/Vintage.json.gz"), dst = DstFile "mtg.json" } ]
>>> destinations = Map.keys  kvs
>>> sources      = Map.elems kvs
>>> length (nub destinations)
3
>>> length (nub sources)
2
>>> DstSrcs kvs
DstSrcs (fromList [(DstStdout,SrcStdin),(DstFile "mtg.json",SrcUri "https://mtgjson.com/json/Vintage.json.gz"),(DstFile "vintage.json",SrcUri "https://mtgjson.com/json/Vintage.json.gz")])

== Definition

`toDstSrcs` should be equivalent to @(`toDstSrcsM` `defaultCheckDstSrcs`)@.

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

>>> :set -XOverloadedStrings
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
--------------------------------------------------

{- | Distinguish `LocalSrc`s from `RemoteSrc`s.

Inverted by `unifySrcs`.

== Laws

@
∀ xs.  sort xs ≡ sort ((uncurry unifySrcs) (partitionSrcs xs))
@

-}

partitionSrcs :: [Src] -> ( RemoteSrcs, LocalSrcs )
partitionSrcs sources = ( fromList remotes, fromList locals )
  where

  ( remotes, locals ) = partitionEithers sources'

  sources' = fromSrc <$> sources

--------------------------------------------------

{- | Generalize `LocalSrc`s and `RemoteSrc`s (to `Src`s).

Inverts `partitionSrcs`.

-}

unifySrcs :: RemoteSrcs -> LocalSrcs -> [Src]
unifySrcs (RemoteSrcs remotes) (LocalSrcs locals) = sources
  where

  sources = locals' <> remotes'

  remotes' = (fromRemoteSrc <$> Set.toList remotes)
  locals'  = (fromLocalSrc  <$> Set.toList locals)

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
  LocalSrcStdin     -> SrcStdin
  LocalSrcFile  fp  -> SrcFile fp

--------------------------------------------------

{- | Generalize a `RemoteSrc`. -}

fromRemoteSrc :: RemoteSrc -> Src
fromRemoteSrc = \case

  RemoteSrcUri url -> SrcUri url

--------------------------------------------------
-- Functions: Validation -------------------------
--------------------------------------------------

{- | Create a `DstSrcs` from multiple `Dst`s and `Src`s.

See `toDstSrcsWithM`.

== Examples

>>> toDstSrcsM []
DstSrcs (fromList [])

== Definition

@
≡ `toDstSrcsWithM` `defaultCheckDstSrcs`
@

-}

toDstSrcsM :: ( MonadThrow m ) => [SrcDst] -> m DstSrcs
toDstSrcsM = toDstSrcsWithM defaultCheckDstSrcs

--------------------------------------------------

{- | Create a `DstSrcs` from multiple `Dst`s and `Src`s, with /lenient/ checking.

See `toDstSrcsWithM`.

== Examples

>>> toDstSrcsM []
DstSrcs (fromList [])

== Definition

@
≡ `toDstSrcsWithM` `lenientCheckDstSrcs`
@

-}

toDstSrcsLenientlyM :: ( MonadThrow m ) => [SrcDst] -> m DstSrcs
toDstSrcsLenientlyM = toDstSrcsWithM lenientCheckDstSrcs

--------------------------------------------------

{- | Create a `DstSrcs` from multiple `Dst`s and `Src`s, with /stringent/ checking.

See `toDstSrcsWithM`.

== Examples

>>> toDstSrcsM []
DstSrcs (fromList [])

== Definition

@
≡ `toDstSrcsWithM` `stringentCheckDstSrcs`
@

-}

toDstSrcsStringentlyM :: ( MonadThrow m ) => [SrcDst] -> m DstSrcs
toDstSrcsStringentlyM = toDstSrcsWithM stringentCheckDstSrcs

--------------------------------------------------

{- | Create a `DstSrcs` from multiple `Dst`s and `Src`s, checking `CheckDstSrcs`.

== Validation

`toDstSrcsWithM` validates the `SrcDst`s by:

* Whether collisions are invalid or ignored.
* Whether the standard handles (i.e. `SrcStdin` and `DstStdout`) are unique.

== Examples

>>> toDstSrcsWithM defaultCheckDstSrcs []
DstSrcs (fromList [])
>>> toDstSrcsWithM lenientCheckDstSrcs []
DstSrcs (fromList [])
>>> toDstSrcsWithM stringentCheckDstSrcs []
DstSrcs (fromList [])

-}

toDstSrcsWithM

  :: forall m. ( MonadThrow m
         )
  => CheckDstSrcs
  -> [SrcDst]
  -> m DstSrcs

toDstSrcsWithM CheckDstSrcs{..} srcdsts' = do

  check dstsrcs

  where

  ------------------------------

  -- remove redundant « SrcDst »s:

  srcdsts :: [SrcDst]
  srcdsts = srcdsts' & ordNub

  dstsrcs :: DstSrcs
  dstsrcs = toDstSrcs srcdsts

  ------------------------------

  srcsList :: [Src]
  srcsList = srcdsts & fmap (\SrcDst{ src } -> src)

  dstsList :: [Dst]
  dstsList = srcdsts & fmap (\SrcDst{ dst } -> dst)

  -- srcsSet :: Set Src
  -- srcsSet = srcsList & Set.fromList

  -- dstsSet :: Set Dst 
  -- dstsSet = dstsList & Set.fromList

  ------------------------------

  check = checkCollisions >=> checkDuplicates >=> checkSecurity -- TODO convert these kleisli arrows to simple predicates, and return all errors [not just the first] via « SrcDstError », a custom exception.

  checkCollisions :: DstSrcs -> m DstSrcs
  checkCollisions dss = case dstCollision of

      IgnoreCollisions   -> return dss
      ProhibitCollisions -> do

          if   noCollision
          then return dss
          else errorM sCollisions

  checkDuplicates :: DstSrcs -> m DstSrcs
  checkDuplicates dss = case dstUniqueness of

         IgnoreDuplicateHandles -> return dss
         AllowDuplicateHandles  -> return dss -- TODO don't use « dss », use a different pipeline that doesn't remove duplicates.
         RequireUniqueHandles   -> do

             if   noDuplicateHandle
             then return dss
             else errorM sDuplicates

  checkSecurity :: DstSrcs -> m DstSrcs
  checkSecurity dss = case srcSecurity of

      AllowHTTP    -> return dss
      RequireHTTPS -> do

          if   allHTTPS
          then return dss
          else errorM sSecurity

  ------------------------------

  noCollision :: Bool
  noCollision = allSingletons groupedDstsBySrc

  noDuplicateHandle :: Bool
  noDuplicateHandle = case dstStdouts of

      []          -> True
      [DstStdout] -> True
      _           -> False

      where

      dstStdouts  = dstsList & filter isDstStdout
      isDstStdout = \case
         DstStdout -> True
         _         -> False

  allHTTPS :: Bool
  allHTTPS
    = srcsList
    & all (\case
            SrcUri url -> isUrlHttps url
            _          -> True)

  ------------------------------

  groupedDstsBySrc :: [[SrcDst]]
  groupedDstsBySrc
    = srcdsts
    & List.groupBy (\SrcDst{ dst = x } SrcDst{ dst = y } -> x == y)

  -- group `Dst`s by their `Src`, then check for only singletons.

  ------------------------------

  allSingletons :: (Eq a) => [[a]] -> Bool
  allSingletons = List.all (\xs -> length xs <= 1)

  ------------------------------

  sCollisions :: String
  sCollisions = "Colliding destinations"  -- TODO format.

  sDuplicates :: String
  sDuplicates = "Duplicate destination handles"  -- TODO format.

  sSecurity :: String
  sSecurity = "Insecure protocol"  -- TODO format.

--------------------------------------------------
-- Functions: Printing / Parsing -----------------
--------------------------------------------------

{- | Parse a (human-readable) `Src`.

`parseSrc` tries these parsers (from top to bottom):

* `parseSrcAsHandle`
* `parseSrcAsFile`
* `parseSrcAsUrl`

`parseSrc` defaults to a `SrcFile`:

* `defaultSrc`

== Examples

e.g. /handle/s:

>>> parseSrc "-"
SrcStdin
>>> parseSrc "stdin"
SrcStdin

e.g. /filepath/s:

>>> parseSrc "./mtg.json"
SrcFile "./mtg.json"
>>> parseSrc "          ./mtg.json          "
SrcFile "./mtg.json"

e.g. /URI/s:

>>> parseSrc "https://mtgjson.com/json/AllCards.json.gz"
SrcUri (URL "https://mtgjson.com/json/AllCards.json.gz")
>>> parseSrc "http://mtgjson.com/json/AllCards.json.gz"
SrcUri (URL "http://mtgjson.com/json/AllCards.json.gz")
>>> parseSrc "mtgjson.com/json/AllCards.json.gz"
SrcUri (URL "https://mtgjson.com/json/AllCards.json.gz")

e.g. Guess whether a suffix a /File Extension/ or a /Top-Level Domain/ (“TLD”):

>>> parseSrc "mtg.json"         -- guess a File ("json" is an uncommon TLD)
SrcFile "./mtg.json"
>>> parseSrc "mtg.org"          -- guess a URI ("com" is a common TLD)
SrcUri (URL "GET https://mtg.org")

e.g. Unabmiguous parsing (no guessing) via /URI Protocol/:

>>> parseSrc "file://mtgjson.com/json/AllCards.json.gz"
SrcFile "mtgjson.com/json/AllCards.json.gz"
>>> parseSrc "https://./mtg.json"
SrcUri (URL "https://mtg.json")

e.g. Unabmiguous parsing (no guessing) via /Filepath Literals/:

>>> parseSrc "mtg.org"
SrcUri (URL "https://mtg.org")
>>> parseSrc "./mtg.org"
SrcFile "./mtg.org"

-}

parseSrc :: String -> Src
parseSrc = munge > go

  where

  go s = guess s & maybe (defaultSrc s) id

  guess s = parseSrcAsHandle s <|> parseSrcAsFile s <|> parseSrcAsUrl s

  -- NOTE: --
  -- « instance Alternative (Maybe _) » picks the first (leftmost) « Just ».

  munge = lrstrip

--------------------------------------------------

{- | Parse a `SrcFile` (if able).

Guesses are based on the:

* Prefix — looks like a /filepath literal/ (e.g. @"./_"@ org @"C:\\_"@).
* Suffix — is a (common) /file extension/ (e.g. @"_.tar.gz"@).

-}

parseSrcAsFile :: String -> Maybe Src
parseSrcAsFile s = Just _

  _ -> Nothing

--------------------------------------------------

{- | Parse a `SrcUri` (if able).

Guesses are based on the:

* Prefix — is a (standard) /HTTP Method/, separated by whitespace (e.g. @"GET _"@).
* Prefix — is a (common) /URI Scheme/ for the Internet (e.g. @"http://_"@).
* Suffix — is a (common) /Top-Level Domain/ (e.g. @"_.com"@).

-}

parseSrcAsUrl :: String -> Maybe Src
parseSrcAsUrl s = Just _

  _ -> Nothing

--------------------------------------------------

{- | Parse a `SrcStdin` (if able).

-}

parseSrcAsHandle :: String -> Maybe Src
parseSrcAsHandle = toLower > \case

  "-"     -> Just SrcStdin
  "stdin" -> Just SrcStdin

  _ -> Nothing

  where

  toLower = fmap Char.toLower

--------------------------------------------------

{- | -}

defaultSrc :: String -> Src
defaultSrc = SrcFile

-- defaultSrc = toFile > SrcFile
-- defaultSrc = toUrl > SrcUri

--------------------------------------------------

{- | Parse a (human-readable) `Dst`.

== Examples

>>> parseDst "-"
DstStdout
>>> parseDst "stdout"
DstStdout

>>> parseDst "./mtg.hs"
DstFile "./mtg.hs"
>>> parseDst "          ./mtg.hs          "
DstFile "./mtg.hs"

-}

parseDst :: String -> Dst
parseDst = munge > \case

  "-"      -> DstStdout
  "stdout" -> DstStdout

  s -> DstFile s

  where

  munge = lrstrip

--------------------------------------------------

{- | Like `parseSrc`, but doesn't parse @URL@s.

== Examples

>>> parseLocalSrc "-"
LocalSrcStdin
>>> parseLocalSrc "stdin"
LocalSrcStdin

>>> parseLocalSrc "./mtg.json"
LocalSrcFile "./mtg.json"
>>> parseLocalSrc "          ./mtg.json          "
LocalSrcFile "./mtg.json"

-}

parseLocalSrc :: String -> LocalSrc
parseLocalSrc = munge > \case

  "-"     -> LocalSrcStdin
  "stdin" -> LocalSrcStdin

  s -> LocalSrcFile s

  where

  munge = lrstrip

--------------------------------------------------

{- | Like `parseSrc`, but doesn't parse @filepath@s / @handle@s.

== Examples

>>> parseRemoteSrc "-"
RemoteSrcUri (URL "-")
>>> parseRemoteSrc "./mtg.json"
RemoteSrcUri (URL "./mtg.json")

>>> parseRemoteSrc ""
RemoteSrcUri (URL "")
>>> parseRemoteSrc ""
RemoteSrcUri (URL "")

-}

parseRemoteSrc :: String -> RemoteSrc
parseRemoteSrc = munge > fromString > RemoteSrcUri

  where

  munge = lrstrip

--------------------------------------------------

prettySrc :: (IsString string) => Src -> string
prettySrc = go > fromString
  where

  go = \case

   SrcBytes  bs -> LazyASCII.unpack   bs
   SrcBytes' bs -> StrictASCII.unpack bs

   SrcStdin   -> "<<stdin>>"
   SrcFile fp -> "" <> fp
   SrcUri url -> "" <> Text.unpack (fromURL url)

--------------------------------------------------

prettyDst :: (IsString string) => Dst -> string
prettyDst = go > fromString
  where

  go = \case

   DstStdout  -> "<<stdout>>"
   DstFile fp -> "" <> fp

--------------------------------------------------
-- Functions: URLs -------------------------------
--------------------------------------------------

{- | /Smart Constructor/ for `URL`s. -}

toUrl :: String -> URL
toUrl s = URL t
  where

  t = mungeUrl (Text.pack s)

  mungeUrl :: Text -> Text
  mungeUrl = Text.strip

--------------------------------------------------

{- |

== Examples

>>> :set -XOverloadedStrings
>>> isUrlHttps "https://mtgjson.com/json/Vintage.json.gz"
True
>>> isUrlHttps "GET https://mtgjson.com/json/Vintage.json.gz"
True

>>> :set -XOverloadedStrings
>>> isUrlHttps "mtgjson.com"
False
>>> isUrlHttps "http://mtgjson.com"
False
>>> isUrlHttps "GET http://mtgjson.com"
False

-}

isUrlHttps :: URL -> Bool
isUrlHttps (URL t) = hasHttpsPrefix t || hasHttpsPrefix t'
  where

  t' = t & Text.dropWhile isWithinUrlMethod

  hasHttpsPrefix :: Text -> Bool
  hasHttpsPrefix = Text.isPrefixOf "https://"

  isWithinUrlMethod :: Char -> Bool
  isWithinUrlMethod c = Char.isUpper c || Char.isSpace c 

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

--TODO-- ordered-containers Data-Set-Ordered

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- 

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------