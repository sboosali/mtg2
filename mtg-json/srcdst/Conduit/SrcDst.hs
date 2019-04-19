--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{- | Sources and Destinations.

-}

module Conduit.SrcDst

  ( -- * `Conduit` for `SrcDst`s (and `DstSrc`)

    conduitSrcDst
  , conduitDstSrcs

  -- * `IO` for `SrcDst`s

  , runSrcDst
  , runSrcDstM

  -- * `Conduit`s for `Src`s

  , conduitSrc
  , conduitSrcBytes'
  , conduitSrcBytes
  , conduitSrcStdin
  , conduitSrcFile
  , conduitSrcUri

  -- * `Conduit`s for `Dst`s

  , conduitDst
  , conduitDstStdin
  , conduitDstFile

  -- * `Conduit`s for `RemoteSrc`s

  , conduitRemoteSrcs

  -- * Miscellaneous `Conduit`s

  , conduitDirectory
  , FollowSymlinks(..)

  -- * `IO` actions for `SrcDst`s

  , readSrc
  , copySrc

  -- * Re-export `SrcDst`, `Src`, and `Dst`

  , module Data.SrcDst

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Data.SrcDst 
import Prelude.SrcDst 

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "http-conduit" Network.HTTP.Conduit as HTTP.Conduit
import qualified "http-conduit" Network.HTTP.Simple  as HTTP.Conduit
-- import           "http-conduit" Network.HTTP.Simple ( )

--------------------------------------------------

import qualified "http-client"     Network.HTTP.Client       as HTTP
import qualified "http-client-tls" Network.HTTP.Client.TLS   as HTTPS
-- import qualified "http-types"      Network.HTTP.Types.Status as HTTP

--------------------------------------------------

import qualified "conduit" Conduit as Conduit
import           "conduit" Conduit ( ConduitT, Sink, (.|), ZipSink(..) )

--------------------------------------------------

import qualified "resourcet" Control.Monad.Trans.Resource as Resource
import           "resourcet" Control.Monad.Trans.Resource ( MonadResource, MonadUnliftIO, ResourceT )

--------------------------------------------------

-- import qualified "zlib" Codec.Compression.GZip     as GZIP    -- GZIP format 
-- import qualified "zlib" Codec.Compression.Zlib     as ZLIB    -- ZLIB format 
-- import qualified "zlib" Codec.Compression.Zlib.Raw as DEFLATE -- DEFLATE format 
-- import qualified "zlib" Codec.Compression.Zlib.Raw as Z

-- import qualified "tar" Codec.Archive.Tar as TAR

-- import qualified "zip-archive" Codec.Archive.Zip as ZIP

--------------------------------------------------

import qualified "time" Data.Time.LocalTime as Time
import qualified "time" Data.Time.Format    as Time

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "containers" Data.Map as Map
import qualified "containers" Data.Set as Set

--------------------------------------------------

import qualified "bytestring" Data.ByteString            as Strict
import qualified "bytestring" Data.ByteString.Lazy       as Lazy

--------------------------------------------------

import qualified "base" System.IO as IO
import qualified "base" Data.List as List

-- import qualified "base" Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | Whether @symlink@s to directories are followed.

-}


data FollowSymlinks

  = FollowSymlinks
  | ConcludeSymlinks

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (GEnum)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'FollowSymlinks'@

instance Default FollowSymlinks where def = FollowSymlinks

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | “Run” a `SrcDst`, streaming the source `Src` towards the destination `Dst`.

== Definition

Specializes `runSrcDstM` at @(m ~ `IO`)@:

@
runSrcDst = `runSrcDstM`
@

-}

runSrcDst :: SrcDst -> IO ()
runSrcDst = runSrcDstM

--------------------------------------------------

{- | “Run” a `SrcDst`, streaming the source `Src` towards the destination `Dst`.

Calls `Conduit.runConduitRes`, which is a @bracket@-like operation,
thus the resource usage of `runSrcDstM` is exception-safe.
That is, these resources are closed:

* File @Handle@s
* Sockets

whether the operation:

* finishes successfully,
* or aborts unsuccessfully.

-}

runSrcDstM
  :: forall m.
    ( MonadIO       m
    , MonadThrow    m
    , MonadUnliftIO m
    )
  => SrcDst
  -> m ()

runSrcDstM srcdst = Conduit.runConduitRes mSrcDst
  where

  mSrcDst = conduitSrcDst srcdst

--------------------------------------------------
-- Functions: Conduit ----------------------------
--------------------------------------------------

{- | Create a “closed” @conduit@, from a source `Src` to a destination `Dst`.

Fuses `conduitSrc` onto `conduitDst`.

Generalizes `runSrcDstM`.

-}

conduitSrcDst
  :: forall m.
    ( MonadResource m
    , MonadIO       m
    , MonadThrow    m
    )
  => SrcDst
  -> ConduitT () Void m ()

conduitSrcDst SrcDst{ src, dst } = mSrc .| mDst
  where

  mSrc = conduitSrc src
  mDst = conduitDst dst

--------------------------------------------------
--------------------------------------------------

{- | Create an “open” @conduit@ /Source/, from the source `Src`.

-}

conduitSrc
  :: forall m.
    ( MonadResource m
    , MonadIO       m
    , MonadThrow    m
    )
  => Src
  -> ConduitT () ByteString m ()

conduitSrc = \case

  SrcBytes  bs  -> conduitSrcBytes  bs
  SrcBytes' bs' -> conduitSrcBytes' bs'

  SrcStdin   -> conduitSrcStdin
  SrcFile fp -> conduitSrcFile fp
  SrcUri url -> conduitSrcUri url

--------------------------------------------------

{- | (See `conduitSrc`.) -}

conduitSrcBytes'
  :: ( MonadResource m, MonadIO m ) => Strict.ByteString -> ConduitT () ByteString m ()
conduitSrcBytes' = Conduit.yield

--------------------------------------------------

{- | (See `conduitSrc`.) -}

conduitSrcBytes
  :: ( MonadResource m, MonadIO m ) => Lazy.ByteString -> ConduitT () ByteString m ()
conduitSrcBytes = Conduit.sourceLazy

--------------------------------------------------

{- | (See `conduitSrc`.) -}

conduitSrcStdin
  :: ( MonadResource m, MonadIO m ) => ConduitT () ByteString m ()
conduitSrcStdin = Conduit.stdinC

--------------------------------------------------

{- | (See `conduitSrc`.) -}

conduitSrcFile
  :: ( MonadResource m, MonadIO m ) => FilePath -> ConduitT () ByteString m ()
conduitSrcFile = Conduit.sourceFile

--------------------------------------------------

{- | (See `conduitSrc`.) -}

conduitSrcUri
  :: ( MonadResource m, MonadIO m, MonadThrow m ) => URL -> ConduitT () ByteString m ()
conduitSrcUri (URL url) = do

  request <- HTTP.parseRequest url

  HTTP.Conduit.httpSource request consume

  where

  consume :: HTTP.Response (ConduitT () ByteString m ()) -> ConduitT () ByteString m ()
  consume response = do    -- TODO -- print response status to stderr (and headache content heading too?) given verbosity.
      -- liftIO $ putStdErr (show (HTTP.getResponseStatus response, HTTP.getResponseHeaders response))
      HTTP.Conduit.getResponseBody response

--------------------------------------------------
--------------------------------------------------

{- | Create an “open” @conduit@ /Sink/, into the destination `Dst`.

-}

conduitDst
  :: ( MonadResource m
    , MonadIO       m
    )
  => Dst
  -> ConduitT ByteString Void m ()

conduitDst = \case

  DstStdout  -> conduitDstStdin
  DstFile fp -> conduitDstFile fp

--------------------------------------------------

{- | (See `conduitDst`.) -}

conduitDstStdin
  :: ( MonadResource m, MonadIO m ) => ConduitT ByteString Void m ()
conduitDstStdin = Conduit.stdoutC

--------------------------------------------------

{- | (See `conduitDst`.) -}

conduitDstFile
  :: ( MonadResource m, MonadIO m ) => FilePath -> ConduitT ByteString Void m ()
conduitDstFile = Conduit.sinkFileCautious

--------------------------------------------------
-- Functions: Conduit ----------------------------
--------------------------------------------------

{- | Create a “closed” @conduit@, from multiple `Src`s to multiple `Dst`s.

`(conduitDstSrcs ...)` is similar to `(traverse conduitDstSrc ...)`, except:

* HTTP Connection Management — For multiple `SrcUri`s, the different files are downloaded under a single `HTTP.Manager`;
this is useful ① for faster downloads (especially when downloading from the same subdomain)
via @keep-alive@, and ② for rate-limiting (I think).
* Source Caching — When fetching the same `Src` to *multiple* `Dst`s, the input bytes are forked to each output
(not re-fetched individually) via `ZipSink`.

-}

conduitDstSrcs
  :: forall m.
    ( MonadResource m
    , MonadIO       m
    , MonadThrow    m
    )
  => DstSrcs
  -> ConduitT () Void m ()

conduitDstSrcs (DstSrcs dstsrcs) = _

  where

  ------------------------------

  srcdsts :: Map Src (NonEmpty Dst)
  srcdsts = dstsrcs & invertMap

  srcdsts1 :: [( Src, ConduitT ByteString Void m () )]
  srcdsts1 = srcdsts
    & Map.map dstConsumer
    & Map.toList

  srcdsts2 :: [( ConduitT () ByteString m (), ConduitT ByteString Void m () )]
  srcdsts2 = srcdsts1
    & _

  ------------------------------

  dstConsumer :: NonEmpty Dst -> ConduitT ByteString Void m ()
  dstConsumer
    = fmap conduitDst
    > toList
    > sequenceSinks_

    -- cache `Src`s by zipping `Dst`s.

  ( remoteSrcs, localSrcs ) = partitionSrcs srcs

  remoteSrcProducers = remoteSrcs
    & conduitRemoteSrcs

--------------------------------------------------
--------------------------------------------------

{- | Create an “producer” for each remote sources.

For /HTTP/ sources, all producers share the same /manager/.

-}

conduitRemoteSrcs
  :: forall m.
    ( MonadResource m
    , MonadIO       m
    , MonadThrow    m
    )
  => RemoteSrcs
  -> m (Map RemoteSrc (ConduitT () ByteString m ()))

conduitRemoteSrcs (RemoteSrcs srcs) = do

  (go manager) srcs

  where

  go manager = \case

    RemoteSrcUri url -> conduitSrcUriWith manager url

--------------------------------------------------
--------------------------------------------------

{- |

== Examples

@conduitDirectory `FollowSymlinks` "./"@ acts like @find -L ./@

-}

conduitDirectory
  :: ( MonadResource m
    )

  => FollowSymlinks

     -- ^ Follow directory symlinks

  -> FilePath

     -- ^ Root directory

  -> ConduitT i FilePath m ()

conduitDirectory followSymlinks = Conduit.sourceDirectoryDeep (fromFollowSymlinks followSymlinks)

--------------------------------------------------
-- Functions: IO ---------------------------------
--------------------------------------------------

{- | Read bytes “lazily” (`LazyBytes`) from a given source (`Src`).

-}

readSrc :: Src -> IO LazyBytes
readSrc = \case

  SrcBytes  bs -> return bs
  SrcBytes' bs -> return (Lazy.fromChunks [bs])

  SrcStdin   -> readSrcStdin
  SrcFile fp -> Lazy.readFile fp
  SrcUri url -> do

    fp <- newTemporaryFilePath Nothing (fromURL url)

    fetch Nothing url fp
    Lazy.readFile fp

--------------------------------------------------

{- | Copy the given source (`Src`) to a file (`FilePath`).

-}

copySrc :: FilePath -> Src -> IO ()
copySrc fpDst = \case

  SrcBytes  bsSrc -> Lazy.writeFile fpDst bsSrc
  SrcBytes' bsSrc -> Strict.writeFile fpDst bsSrc

  SrcStdin      -> readSrcStdin >>= Lazy.writeFile fpDst
  SrcFile fpSrc -> Directory.copyFile fpSrc fpDst
  SrcUri uriSrc -> fetch Nothing uriSrc fpDst

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

timeFormatWithHyphensAndUnits :: String
timeFormatWithHyphensAndUnits = "%Y-%m-%d-%Hh-%Mm-%Ss-%03qms"

-- NOTE given the meta-syntax « %<modifier><width><alternate><specifier> »,
--      the syntax « %03q » means (0-padded) 3-width picoseconds (i.e. milliseconds).

--------------------------------------------------

replacedCharacters_escapeFilePath :: Set Char
replacedCharacters_escapeFilePath = Set.fromList "/\\ _:;'\"!#$%^&*?|[]{}()"

--------------------------------------------------

replacementCharacter_escapeFilePath :: Char
replacementCharacter_escapeFilePath = '_'

--------------------------------------------------

default_escapeFilePath :: FilePath
default_escapeFilePath = "file"

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

fromFollowSymlinks :: FollowSymlinks -> Bool
fromFollowSymlinks = \case

  FollowSymlinks   -> True
  ConcludeSymlinks -> False
  
--------------------------------------------------

{- | Read bytes “lazily” (`LazyBytes`) from @stdin@ (`IO.stdin`).

-}

readSrcStdin :: IO LazyBytes
readSrcStdin = do
  IO.hSetBinaryMode IO.stdin True
  Lazy.hGetContents IO.stdin

--------------------------------------------------

{- | Download a file.

@fetch method uri fp@ downloads URI @uri@ to FilePath @fp@ (with optional request method @method@).

The file contents being downloaded may be larger than available memory. @uri@ is streamed into @fp@.

== Examples

@
>> fetch (Just "GET") "https://mtgjson.com/json/Vintage.json.gz" "/tmp/Vintage.json.gz"
@

-}

fetch :: Maybe String -> URL -> FilePath -> IO ()
fetch method (URL url) fp = do

  request <- HTTP.parseRequest url'

  Resource.runResourceT (HTTP.Conduit.httpSink request consume)

  where

  method' = method & maybe "" (++ " ")
  url'    = method' <> url

  consume :: HTTP.Response () -> ConduitT ByteString Void (ResourceT IO) ()
  consume _response = do    -- TODO -- print response status to stderr (and headache content heading too?) given verbosity.

    Conduit.sinkFileCautious fp

--------------------------------------------------

{- | Download and decompress a @JSON@ file.

== Exceptions

May throw:

* `HTTP.HttpException`

== Related

* `conduitRemoteSrcWith`
* `conduitDstSrcs`

-}

conduitRemoteSrc

  :: forall m. ( MonadResource m, MonadIO m, MonadThrow m )
  => URL
  -> m (HTTP.Manager, ConduitT () ByteString m ())

conduitRemoteSrc url = do

  let settings = HTTPS.tlsManagerSettings

  manager <- HTTPS.newTlsManagerWith settings

  producer <__conduitRemoteSrcWith manager url

  return ( manager, producer )

{-# INLINEABLE conduitRemoteSrc #-}

--------------------------------------------------

{- | (See `conduitRemoteSrc`.) -}

conduitRemoteSrcWith

  :: forall m. ( MonadResource m, MonadIO m, MonadThrow m )
  => HTTP.Manager
  -> URL
  -> m (ConduitT () ByteString m ())

conduitRemoteSrcWith manager = go
  where

  go :: URL -> m (ConduitT () ByteString m ())
  go (URL url) = do

    request  <- HTTP.parseUrlThrow url
    response <- HTTP.Conduit.http request manager

    let producer = (response & HTTP.Conduit.getResponseBody)
    let status   = (response & HTTP.Conduit.getResponseStatus)

    return producer

{-# INLINEABLE conduitRemoteSrcWith #-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- | @mkdir -p@

mkdir_p :: FilePath -> IO ()
mkdir_p = Directory.createDirectoryIfMissing True

--------------------------------------------------

invertMap :: (Ord k, Ord v) => Map k v -> Map v (NonEmpty k)
invertMap kvs = vkss
  where

  vkss = go kvs

  go
    = Map.toList
    > fmap (\(k, v) -> (v, k:|[]))
    > Map.fromListWith (<>)

--------------------------------------------------
-- Utilities: Time -------------------------------
--------------------------------------------------

{- | Application-specific filepath to a temporary file.

Creates any missing parent directories.

e.g.:

@
> newTemporaryFilePath (Just "mtg-json") "mtg.json.gz"
"/tmp/mtg-json/2019-04-06-21h-43m-51s-852ms_mtg.json.gz"

> newTemporaryFilePath Nothing ""
"/tmp/2019-04-06-21h-43m-51s-852ms_file"
@

-}

newTemporaryFilePath
  :: Maybe String
  -- ^ Optional directory name (for “namespacing”).
  -> String
  -- ^ File name (suffix).
  -> IO FilePath
  -- ^ Generated filepath.

newTemporaryFilePath directoryName fileName = do

  directory <- Directory.getTemporaryDirectory
  time      <- Time.getZonedTime

  let timestamp = formatZonedTimeAsFilePath time

  let dirname  = (maybe directory (directory </>)) dName
  let basename = timestamp <> "_" <> fName 

  mkdir_p dirname

  let path = dirname </> basename

  return path

  where

  fName = escapeFilePath fileName
  dName = escapeFilePath <$> directoryName

--------------------------------------------------

{- | Escape the given string as both a /valid/ and /idiomatic/ filepath.

== Examples

>>> escapeFilePath ""
"file"

>>> escapeFilePath "https://mtgjson.com/json/Vintage.json.gz"
"https_mtgjson.com_json_Vintage.json.gz"

-}

escapeFilePath :: String -> FilePath
escapeFilePath s = fp
  where

  fp :: FilePath
  fp = case s of

    "" -> default_escapeFilePath
    _  -> go s

  go
    = applyReplacements
    > List.group
    > fmap collapseReplacementCharacters
    > concat

  applyReplacements :: String -> String
  applyReplacements = fmap applyReplacement

  applyReplacement :: Char -> Char
  applyReplacement c =
    if   c `Set.member` replacedCharacters_escapeFilePath
    then replacementCharacter_escapeFilePath
    else c

  collapseReplacementCharacters :: String -> String
  collapseReplacementCharacters t =

    if   List.all (== replacementCharacter_escapeFilePath) t
    then [replacementCharacter_escapeFilePath]
    else t

--------------------------------------------------

{- | Format a timestamp to be part of a filepath.

e.g.:

@
> formatZonedTimeAsFilePath _
"2019-04-06-21h-43m-51s-852ms"
@

== Examples

>>> import Data.Time.LocalTime ( ZonedTime(..), TimeZone(..), LocalTime(..), TimeOfDay(..) )
>>> import Data.Time.Calendar ( Day(..), fromGregorian )
>>> zone = TimeZone { timeZoneMinutes = -420, timeZoneSummerOnly = True, timeZoneName = "PDT" }
>>> time = LocalTime { localDay = fromGregorian 1991 02 28, localTimeOfDay = TimeOfDay { todHour = 13, todMin = 37, todSec = 59 }}
>>> time' = ZonedTime{ zonedTimeToLocalTime = time, zonedTimeZone = zone }
>>> formatZonedTimeAsFilePath time'
"1991-02-28-13h-37m-59s-000ms"

-}

formatZonedTimeAsFilePath :: Time.ZonedTime -> String
formatZonedTimeAsFilePath t =

  Time.formatTime locale timeFormatWithHyphensAndUnits t

  where

  locale = Time.defaultTimeLocale

  -- NOTE even « Prelude.undefined » works as the locale for « ZonedTime » (it's ignored).

--------------------------------------------------
-- Utilities: Conduit ----------------------------
--------------------------------------------------

{- | n-ary `Conduit.zipSinks` (via `ZipSink`).

== Usage

Stream a single input to multiple outputs.

-}

sequenceSinks :: (Monad m) => [Sink i m r] -> Sink i m [r]
sequenceSinks

  = fmap ZipSink
  > sequenceA
  > getZipSink

--------------------------------------------------

{- | Like `sequenceSinks`, but ignoring the results.

== Usage

Stream a single input to multiple outputs.

-}

sequenceSinks_ :: (Monad m) => [Sink i m ()] -> Sink i m ()
sequenceSinks_

  = fmap ZipSink
  > sequenceA_
  > getZipSink

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- data ConduitT i o m r

-- stdinC :: MonadIO m => ConduitT i ByteString m ()

-- stdoutC :: MonadIO m => ConduitT ByteString o m ()

-- httpSink :: (MonadUnliftIO m) => Request -> (Response () -> ConduitT ByteString Void m a) -> m a

-- httpSource :: (MonadResource m, MonadIO n) => Request -> (Response (ConduitT i ByteString n ()) -> ConduitT i o m r) -> ConduitT i o m r

-- TimeZone { timeZoneMinutes = , timeZoneSummerOnly = False, timeZoneName = "PST" }

-- "duplicateConsumer" is called `zipSinks`:
--
-- zipSinks :: Monad m => Sink i m r -> Sink i m r' -> Sink i m (r, r')
--
-- >Combines two sinks. The new sink will complete when both input sinks have completed.

-- traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
--
-- traverseMaybeWithKey :: Applicative f => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)

-- mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a

--- Network.HTTP.Conduit:
--
-- http :: MonadResource m => Request -> Manager -> m (Response (ConduitT i ByteString m ()))
--
-- getRedirectedRequest :: Request -> ResponseHeaders -> CookieJar -> Int -> Maybe Request
--
-- getResponseHeader :: HeaderName -> Response a -> [ByteString]
--
-- multipleChoices300 :: Status
-- movedPermanently301 :: Status
-- found302 :: Status
-- seeOther303 :: Status
-- notModified304 :: Status
-- useProxy305 :: Status
-- -- 306
-- temporaryRedirect307 :: Status
-- permanentRedirect308 :: Status
-- 
-- https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
--
-- The HyperText Transfer Protocol (HTTP) 301 Moved Permanently redirect status response code indicates that the resource requested has been definitively moved to the URL given by the Location headers. A browser redirects to this page and search engines update their links to the resource (in 'SEO-speak', it is said that the 'link-juice' is sent to the new URL).
-- It's recommended to use the 301 code only as a response for GET or HEAD methods and to use the 308 Permanent Redirect for POST methods instead.
-- 
-- The HyperText Transfer Protocol (HTTP) 302 Found redirect status response code indicates that the resource requested has been temporarily moved to the URL given by the Location header.
-- It's recommended to set the 302 code only as a response for GET or HEAD methods and to use 307 Temporary Redirect instead.
-- 
-- 
-- 
-- 
-- 

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------