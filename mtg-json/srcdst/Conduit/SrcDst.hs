--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{- | Sources and Destinations.

-}

module Conduit.SrcDst

  ( module Data.SrcDst

  , FollowSymlinks(..)

  , conduitSrc
  , conduitDst
  , conduitDirectory

  , readSrc
  , copySrc

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Data.SrcDst 
import Prelude.SrcDst 

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "conduit" Conduit as Conduit
import           "conduit" Conduit ( ConduitM )

--------------------------------------------------

import qualified "http-conduit" Network.HTTP.Simple as HTTP.Conduit
import           "http-conduit" Network.HTTP.Simple ( )

--------------------------------------------------

import qualified "http-types"      Network.HTTP.Types.Status as HTTP
import qualified "http-client"     Network.HTTP.Client       as HTTP
import qualified "http-client-tls" Network.HTTP.Client.TLS   as HTTPS

--------------------------------------------------

import qualified "resourcet" Control.Monad.Trans.Resource as Resource
import           "resourcet" Control.Monad.Trans.Resource ( MonadResource, ResourceT )

--------------------------------------------------

import qualified "zlib" Codec.Compression.GZip     as GZIP    -- GZIP format 
import qualified "zlib" Codec.Compression.Zlib     as ZLIB    -- ZLIB format 
import qualified "zlib" Codec.Compression.Zlib.Raw as DEFLATE -- DEFLATE format 
import qualified "zlib" Codec.Compression.Zlib.Raw as Z

--------------------------------------------------

import qualified "zip-archive" Codec.Archive.Zip as ZIP

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "bytestring" Data.ByteString            as Strict
import qualified "bytestring" Data.ByteString.Lazy       as Lazy
import qualified "bytestring" Data.ByteString.Lazy.Char8 as ASCII

--------------------------------------------------

import qualified "base" System.IO as IO

import qualified "base" Prelude

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
-- Functions: Conduit ----------------------------
--------------------------------------------------

{- | Create a @conduit@ /Source/, from the source `Src`.

-}

conduitSrc
  :: forall m.
    ( MonadResource m
    , MonadIO       m
    )
  => Src
  -> ConduitM () ByteString m ()

conduitSrc = \case

  SrcBytes  bs  -> Conduit.sourceLazy bs
  SrcBytes' bs' -> Conduit.yield bs'

  SrcStdin   -> Conduit.stdinC
  SrcFile fp -> Conduit.sourceFile fp
  SrcUri url -> download url

  where

  download :: URL -> FilePath -> ConduitM () ByteString m ()
  download (URL url) fp = do

      request <- HTTP.parseRequest url

      HTTP.Conduit.httpSource request consume

      where

      consume :: HTTP.Response (ConduitM () ByteString m ()) -> ConduitM () ByteString m ()
      consume response = do    -- TODO -- print response status to stderr (and headache content heading too?) given verbosity.
          -- liftIO $ putStdErr (show (HTTP.getResponseStatus response, HTTP.getResponseHeaders response))
          HTTP.getResponseBody response

-- httpSource :: (MonadResource m, MonadIO n) => Request -> (Response (ConduitM i ByteString n ()) -> ConduitM i o m r) -> ConduitM i o m r

--------------------------------------------------

{- | Create a @conduit@ /Sink/, into the destination `Dst`.

-}

conduitDst
  :: ( MonadIO m
    )
  => Dst
  -> ConduitM ByteString Void m ()

conduitDst = \case

  DstStdout  -> Conduit.stdoutC
  DstFile fp -> Conduit.sinkFileCautious fp

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

  -> ConduitM i FilePath m ()	 

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
  SrcUri uri -> do

    fp <- newTemporaryFilePath uri

    fetch Nothing uri fp
    Lazy.readFile fp

--------------------------------------------------

{- | Copy the given source (`Src`) to a file (`FilePath`).

-}

copySrc :: FilePath -> Src -> IO LazyBytes
copySrc fpDst = \case

  SrcBytes  bsSrc -> Lazy.writeFile fpDst bsSrc
  SrcBytes' bsSrc -> Strict.writeFile fpDst bsSrc

  SrcStdin      -> readSrcStdin >>= Lazy.writeFile fpDst
  SrcFile fpSrc -> Directory.copyFile fpSrc fpDst
  SrcUri uriSrc -> fetch Nothing uriSrc fpDst

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

  consume :: HTTP.Response () -> ConduitM ByteString Void (ResourceT IO) ()
  consume response = do

    Conduit.sinkFileCautious fp

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- data ConduitT i o m r

-- stdinC :: MonadIO m => ConduitT i ByteString m ()

-- stdoutC :: MonadIO m => ConduitT ByteString o m ()

-- httpSink :: (MonadUnliftIO m) => Request -> (Response () -> ConduitM ByteString Void m a) -> m a

-- httpSource :: (MonadResource m, MonadIO n) => Request -> (Response (ConduitM i ByteString n ()) -> ConduitM i o m r) -> ConduitM i o m r

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------