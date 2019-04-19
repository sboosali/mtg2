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

  -- * Re-export `SrcDst` types.

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

-- | a `Conduit.Source` of `BytesString`.

type BytesSource m = ConduitT () ByteString m ()
  
--------------------------------------------------

-- | a `Conduit.Sink` for `BytesString`.

type BytesSink m = ConduitT ByteString Void m ()

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

  manager <- newManager

  let
    mIOs :: Map RemoteSrc (m (BytesSource m))
    mIOs = mSrcs & Map.map (go manager)

  mConduits :: Map RemoteSrc (BytesSource m) <- sequenceA mIOs

  return mConduits

  where

  ------------------------------

  mSrcs :: Map RemoteSrc RemoteSrc
  mSrcs = srcs & Map.fromSet id

  ------------------------------

  go
    :: HTTP.Manager
    -> RemoteSrc -> m (BytesSource m)

  go manager = \case

    RemoteSrcUri url -> (conduitRemoteSrcWith manager url)

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

  manager <- newManager

  producer <- conduitRemoteSrcWith manager url

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

  go :: URL -> m (BytesSource m)
  go (URL url) = do

    request  <- HTTP.parseUrlThrow url
    response <- HTTP.Conduit.http request manager

    let producer = (response & HTTP.Conduit.getResponseBody)
    let status   = (response & HTTP.Conduit.getResponseStatus)

    return producer

{-# INLINEABLE conduitRemoteSrcWith #-}

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
-- Utilities -------------------------------------
--------------------------------------------------

fromFollowSymlinks :: FollowSymlinks -> Bool
fromFollowSymlinks = \case

  FollowSymlinks   -> True
  ConcludeSymlinks -> False

--------------------------------------------------
-- Utilities: HTTP -------------------------------
--------------------------------------------------

newManager
  :: forall m. ( MonadResource m, MonadIO m, MonadThrow m )
  => m HTTP.Manager

newManager = do

  let settings = HTTPS.tlsManagerSettings

  manager <- HTTPS.newTlsManagerWith settings

  return manager

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

--- « Data.Conduit »
--
-- data ConduitT i o m r
--
-- stdinC :: MonadIO m => ConduitT i ByteString m ()
--
-- stdoutC :: MonadIO m => ConduitT ByteString o m ()

-- httpSink :: (MonadUnliftIO m) => Request -> (Response () -> ConduitT ByteString Void m a) -> m a
--
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

-- « Data.Map »
-- 
-- fromSet :: (k -> a) -> Set k -> Map k a
-- 
-- traverseMaybeWithKey :: Applicative f => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
-- 
-- 

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------