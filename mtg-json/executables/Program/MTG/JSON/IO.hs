--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{-|

-}

module Program.MTG.JSON.IO where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.MTG.JSON.Types
import Program.MTG.JSON.Constants
import Program.MTG.JSON.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "zlib" Codec.Compression.GZip     as GZIP    -- GZIP format 
import qualified "zlib" Codec.Compression.Zlib     as ZLIB    -- ZLIB format 
import qualified "zlib" Codec.Compression.Zlib.Raw as DEFLATE -- DEFLATE format 
import qualified "zlib" Codec.Compression.Zlib.Raw as Z

--------------------------------------------------

import qualified "zip-archive" Codec.Archive.Zip as ZIP

--------------------------------------------------

import qualified "http-types"      Network.HTTP.Types.Status as HTTP
import qualified "http-client"     Network.HTTP.Client       as HTTP
import qualified "http-client-tls" Network.HTTP.Client.TLS   as HTTPS

--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ( (%) )

--------------------------------------------------

import qualified "bytestring" Data.ByteString.Lazy       as Lazy
import qualified "bytestring" Data.ByteString.Lazy.Char8 as ASCII

--------------------------------------------------

import qualified "base" System.IO as IO

import qualified "base" Prelude

--------------------------------------------------
-- IO --------------------------------------------
--------------------------------------------------

{- | Any @mtg-json@ invocation executes @runCommand@.

== CLI

@
$ mtg-json ...
@

-}

runCommand :: Command -> IO ()
runCommand Command{ subcommand, options } = go subcommand
  where

  go :: Subcommand -> IO ()
  go = \case

      FetchJSON srcdst -> do
        fetchJSON options srcdst

      PrintVersion -> do
        printVersion options

      PrintLicense -> do
        printLicense options

--------------------------------------------------

{- | 

== CLI

@
$ mtg-json fetch --input ... --output ... version
@

-}

fetchJSON :: Options -> SrcDst -> IO ()
fetchJSON Options{..} SrcDst{src,dst} = do

  putStdErr sSrc

  mtg_json <- inputSrc

  let mtg_hs = mtgjson2mtghs mtg_json

  putStdErr sDst

  outputDst mtg_hs

  where

  ------------------------------

  sSrc :: String
  sSrc = runFormat ("\nFetching from: " % Format.string % " ...\n") s
      where
      s = prettySrc src

  sDst :: String
  sDst = runFormat ("\nSaving to:     " % Format.string % " ...\n") s
      where
      s = prettyDst dst

  ------------------------------

  inputSrc :: IO (MTGJSON String)
  inputSrc = case src of

      SrcStdin -> do

          promptSrc

      SrcLines ts -> do

          return $ MTGJSON (Prelude.unlines (toS <$> ts))

      SrcFile fp -> do

          readSrc fp

      SrcUri uri -> do

          fetchSrc uri

  ------------------------------

  outputDst :: (MTGHS String) -> IO ()
  outputDst mtg_hs = case dst of

      DstStdout -> do

          printDst mtg_hs

      DstFile fp -> do

          writeDst fp mtg_hs

  ------------------------------

  promptSrc :: IO (MTGJSON String)
  promptSrc = MTGJSON <$> do

    IO.hGetContents IO.stdin

    -- IO.hGetContents IO.stdin

  ------------------------------

  readSrc :: FilePath -> IO (MTGJSON String)
  readSrc fp = MTGJSON <$> do

    IO.readFile fp

  ------------------------------

  fetchSrc :: URI -> IO (MTGJSON String)
  fetchSrc uri = MTGJSON <$> do

    IO.readFile uri  -- TODO -- download, decompress, read.

  ------------------------------

  printDst :: (MTGHS String) -> IO ()
  printDst (MTGHS mtg) = putStdOut mtg

  ------------------------------

  writeDst :: FilePath -> (MTGHS String) -> IO ()
  writeDst fp (MTGHS mtg) = case forcefulness of

      RespectExisting -> do

        let e = runFormat ("\n[Error] Filepath {{{ " % Format.string % " }}} already exists. Rerun <<< " % Format.string % " >>> with the <<< --force >>> option to overwrite.\n") fp programExecutable -- TODO -- prompt user to confirm.

        bExists <- Directory.doesPathExist fp
        if   bExists
        then putStdErr e
        else IO.writeFile fp mtg

      OverwriteExisting -> do

        IO.writeFile fp mtg

  ------------------------------

{-# INLINEABLE fetchJSON #-}

--------------------------------------------------

{- | 

== CLI

@
$ mtg-json print version
@

-}

printVersion :: Options -> IO ()
printVersion Options{..} = do

  go verbose

  where

  go = \case

    Quiet   -> printVersionConcise
    Concise -> printVersionConcise

    Verbose -> printVersionVerbose
    Loud    -> printVersionVerbose

  printVersionConcise = do

    putStrLn $ programVersionBranch

  printVersionVerbose = do

    let versionLine = concat [ programName, ", version ", programVersion ]

    putStrLn $ versionLine

--  let versionString = versionStringBranch ++ versionStringTags

{-# INLINEABLE printVersion #-}

--------------------------------------------------

{- | 

== CLI

@
$ mtg-json print license
@

-}

printLicense :: Options -> IO ()
printLicense Options{..} = do

  go verbose

  where

  go = \case

    Quiet   -> printLicenseConcise
    Concise -> printLicenseConcise

    Verbose -> printLicenseVerbose
    Loud    -> printLicenseVerbose

  printLicenseConcise = do

    putStrLn $ programLicenseIdentifier

  printLicenseVerbose = do

    printLicenseConcise
    putStrLn ""

    putStrLn $ programLicenseContents

{-# INLINEABLE printLicense #-}

--------------------------------------------------

{- | Download and decompress a @JSON@ file.

== Exceptions

May throw:

* ``
* ``

-}

fetch :: forall a. FetchConfig a -> IO a
fetch config = do

  let settings = HTTPS.tlsManagerSettings

  manager <- HTTPS.newTlsManagerWith settings

  fetchWith manager config

{-# INLINEABLE fetch #-}

--------------------------------------------------

{- | (See `fetch`.) -}

fetchWith :: forall a. HTTPS.Manager -> FetchConfig a -> IO a
fetchWith manager config = go config
  where

  go :: FetchConfig a -> IO a
  go = \case

    FetchMtgJsonGz uri -> fetchMtgJsonGz uri

  fetchMtgJsonGz :: URI -> IO (MTGJSON a)
  fetchMtgJsonGz uri = do

    _

{-# INLINEABLE fetchWith #-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

mtgjson2mtghs :: MTGJSON t -> MTGHS t
mtgjson2mtghs (MTGJSON s) = (MTGHS s) -- TODO

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------]