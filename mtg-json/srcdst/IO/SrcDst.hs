--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------

{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

--------------------------------------------------

{- | Sources and Destinations.

-}

module IO.SrcDst

  (  -- * `IO` actions on `SrcDst`s.

    readSrc
  , copySrc

  -- * `IO` actions on `SrcUri`s.

  , fetchUrls
  , fetchUrl
  , fetchUrlWith

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

import qualified "http-types"      Network.HTTP.Types        as HTTP
import qualified "http-client"     Network.HTTP.Client       as HTTP
import qualified "http-client-tls" Network.HTTP.Client.TLS   as HTTPS

import           "http-types"      Network.HTTP.Types.Status ( Status(..) )

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ( (%) )

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "containers" Data.Map as Map
-- import qualified "containers" Data.Set as Set

--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------

import qualified "bytestring" Data.ByteString            as Strict
import qualified "bytestring" Data.ByteString.Lazy       as Lazy
import qualified "bytestring" Data.ByteString.Char8      as ASCII

--------------------------------------------------

import qualified "base" System.IO as IO

import qualified "base" Control.Exception as E

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type FetchResult = Either (Maybe HTTP.Status) (URL, Lazy.ByteString)

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
  SrcUri url -> fetchUrl url

--------------------------------------------------

{- | Copy the given source (`Src`) to a file (`FilePath`).

-}

copySrc :: FilePath -> Src -> IO ()
copySrc fpDst = \case

  SrcBytes  bsSrc -> Lazy.writeFile fpDst bsSrc
  SrcBytes' bsSrc -> Strict.writeFile fpDst bsSrc

  SrcStdin      -> readSrcStdin >>= Lazy.writeFile fpDst
  SrcFile fpSrc -> Directory.copyFile fpSrc fpDst
  SrcUri uriSrc -> do

      bsSrc <- fetchUrl uriSrc

      Lazy.writeFile fpDst bsSrc

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

{- | Read bytes “lazily” (`LazyBytes`) from @stdin@ (`IO.stdin`).

-}

readSrcStdin :: IO LazyBytes
readSrcStdin = do
  IO.hSetBinaryMode IO.stdin True
  Lazy.hGetContents IO.stdin

--------------------------------------------------

{- | Download the file at each `URL`.

Returns all successful downlaods
(an output of @Map.empty@ means “nothing was download”.)

The keys of @(`Map` URL `Lazy.ByteString`)@ are a subset of the input @URL@s.

== Usage

@
do
  downlaods <- fetchUrl urls
@

-}

fetchUrls :: [URL] -> IO (Map URL Lazy.ByteString)
fetchUrls urls' = do

  manager <- newManager

  let fetch' url = fetchUrlWith manager url `E.catch` handleFetchError

  let
    fetchings :: [IO FetchResult]
    fetchings = fetch' <$> urls

  allResults <- go `traverse` fetchings

  let goodResults = allResults & catMaybes

  let results = goodResults & Map.fromList

  return results

  where

  urls = urls' & ordNub

  go :: IO FetchResult -> IO (Maybe (URL, Lazy.ByteString))
  go fetching = do

    eResult <- fetching

    let mResult = eResult & either (const Nothing) Just

    return mResult

  handleFetchError :: HTTP.HttpException -> IO FetchResult
  handleFetchError _e = do

    let result = Left Nothing

    return result

{-# INLINEABLE fetchUrls #-}

--------------------------------------------------

{- | Download the file at `URL`.

@fetchUrl method uri fp@ downloads URI @uri@ to FilePath @fp@ (with optional request method @method@).

The file contents being downloaded may be larger than available memory. @uri@ is streamed into @fp@.

== Examples

@
>> fetchUrl (Just "GET") "https://mtgjson.com/json/Vintage.json.gz" "/tmp/Vintage.json.gz"
@

== Definition

@
`fetchUrl` url ≡ `fetchUrls` [url]
@

-}

fetchUrl :: URL -> IO Lazy.ByteString
fetchUrl url = do

  manager <- newManager

  result <- fetchUrlWith manager url

  download <- result & either (errorM . toMessage) return

  let ( _url, body ) = download

  return body

  where

  toMessage = \case

    Nothing -> runFormat ("{{{ fetchUrl }}}: Can't download {{{ " % Format.stext % " }}.")

        (fromURL url)

    Just Status{statusCode,statusMessage} -> runFormat ("{{{ fetchUrl }}}: Can't download {{{ " % Format.stext % " }}}; got HTTP Status Code {{{ " % Format.int % " }}} with message “" % Format.string % "”.")

        (fromURL url)
        statusCode
        (ASCII.unpack statusMessage)

{-# INLINEABLE fetchUrl #-}

--------------------------------------------------

{- | Download the file at `URL`.

== Exceptions

May throw:

* `HTTP.HttpException`

-}

fetchUrlWith :: HTTP.Manager -> URL -> IO FetchResult
fetchUrlWith manager = go
  where

  go :: URL -> IO FetchResult
  go url@(URL tUrl) = do

    let sUrl = Text.unpack tUrl

    request  <- HTTP.parseUrlThrow sUrl
    response <- HTTP.httpLbs request manager

    let body   = (response & HTTP.responseBody)
    let status = (response & HTTP.responseStatus)

    let l = (Left (Just status))
    let r = (Right (url, body))

    if   True
    then return r
    else return l

{-# INLINEABLE fetchUrlWith #-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

newManager :: IO HTTP.Manager
newManager = do

  let settings = HTTPS.tlsManagerSettings

  manager <- HTTPS.newTlsManagerWith settings

  return manager

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- « http-conduit »:
--
-- manager = mkManagerSettings (TLSSettingsSimple True False False) Nothing
--
-- let request = request' { checkStatus = \_ _ _ -> Nothing }
--

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------