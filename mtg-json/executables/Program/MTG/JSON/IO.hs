--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{- |

-}

module Program.MTG.JSON.IO where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Conduit.SrcDst

import Program.MTG.JSON.Types
import Program.MTG.JSON.Paths
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

import qualified "http-conduit" Network.HTTP.Simple as HTTP.Conduit
import           "http-conduit" Network.HTTP.Simple ( )

--------------------------------------------------

import qualified "conduit" Conduit as Conduit
import           "conduit" Conduit ( ConduitT, (.|) )

--------------------------------------------------

import qualified "resourcet" Control.Monad.Trans.Resource as Resource
import           "resourcet" Control.Monad.Trans.Resource ( MonadResource, MonadUnliftIO, ResourceT )

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ( (%) )

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "bytestring" Data.ByteString            as Strict
import qualified "bytestring" Data.ByteString.Lazy       as Lazy
import qualified "bytestring" Data.ByteString.Lazy.Char8 as ASCII

import           "bytestring" Data.ByteString ( ByteString )

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

      Fetch srcdst -> do
        fetchMTG options srcdst

      PrintVersion -> do
        printVersion options

      PrintLicense -> do
        printLicense options

--------------------------------------------------

{- | 

== CLI

@
$ mtg-json fetch --input ... --output ... (all | vintage | ...)
@

TODO: multiple inputs/outputs:

@
$ mtg-json fetch --input=vintage --output=stdout --output=/tmp/vintage.json --input="https://mtgjson.com/json/AllCards.json.gz" --output=/tmp/mtg.json --output=./mtg.json.gz

  # is equivalent to this command (with explicitly paired options):

$ mtg-json fetch  --input=vintage --output=stdout  --input=vintage --output=/tmp/vintage.json  --input="https://mtgjson.com/json/AllCards.json.gz" --output=/tmp/mtg.json  --input="https://mtgjson.com/json/AllCards.json.gz" --output=./mtg.json.gz

  # is equivalent to these commands (though the single command would be faster, as it reuses connections and caches intermediate archives and\/or compressions):

$ mtg-json fetch --input=vintage                                     --output=stdout 
$ mtg-json fetch --input=vintage                                     --output=/tmp/vintage.json
$ mtg-json fetch --input="https://mtgjson.com/json/AllCards.json.gz" --output=/tmp/mtg.json
$ mtg-json fetch --input="https://mtgjson.com/json/AllCards.json.gz" --output=./mtg.json.gz
@

-}

fetchMTG :: Options -> SrcDst -> IO ()
fetchMTG Options{..} SrcDst{src,dst} = do

  putStdErr sSrc
  putStdErr sDst

  case dryrun of

      DryRun  -> runDryly
      TrueRun -> runTruly

  where

  ------------------------------

  runTruly :: IO ()
  runTruly = do

      Conduit.runConduitRes mSrcDst

  ------------------------------

  mSrcDst = mSrc .| mDst

  mSrc = conduitSrc src
  mDst = conduitDst dst

  ------------------------------

  runDryly :: IO ()
  runDryly = do

      when (verbose >= Verbose) do
          putStdErr e

      nothing

      where
      e = runFormat ("\n[INFO] Nothing will be fetched (or written), because this invocation is a “Dry-Run”. Rerun <<< " % Format.string % " fetch ... >>> WITHOUT the <<< --dry-run >>> option to truly fetch.\n") programExecutable

  ------------------------------

  sSrc :: String
  sSrc = runFormat ("\nFetching from: {{{ " % Format.string % " }}}...\n") s
      where
      s = prettySrc src

  sDst :: String
  sDst = runFormat ("\nSaving to:     {{{ " % Format.string % " }}}...\n") s
      where
      s = prettyDst dst

  ------------------------------

  writeDst :: FilePath -> (MTGHS String) -> IO ()
  writeDst fp (MTGHS mtg) = case forcefulness of

      RespectExisting -> do

        let e = runFormat ("\n[ERROR] Filepath {{{ " % Format.string % " }}} already exists. Rerun <<< " % Format.string % " >>> with the <<< --force >>> option to overwrite.\n") fp programExecutable -- TODO -- prompt user to confirm.

        bExists <- Directory.doesPathExist fp
        if   bExists
        then putStdErr e
        else IO.writeFile fp mtg

      OverwriteExisting -> do

        IO.writeFile fp mtg

  ------------------------------

{-# INLINEABLE fetchMTG #-}

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
-- Functions -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

mtgjson2mtghs :: MTGJSON a -> MTGHS a
mtgjson2mtghs (MTGJSON x) = (MTGHS x) -- TODO

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- type Sink i = ConduitT i Void

-- sinkFile :: MonadResource m => FilePath -> ConduitT ByteString o m ()
-- sinkFile :: FilePath -> ConduitT ByteString o IO ()
-- sinkFile :: FilePath -> ConduitT ByteString Void IO ()
-- sinkFile :: FilePath -> Sink ByteString IO ()

-- httpSink :: MonadUnliftIO m => Request -> (Response () -> ConduitT ByteString Void m a) -> m a

-- runResourceT :: MonadUnliftIO m => ResourceT m a -> m a

-- Format.bytes:
--
-- >>> format (bytes shortest) 1024
-- "1KB"
--
-- >>> format (bytes (fixed 2 % " ")) (1024*1024*5)
-- "5.00 MB"
--

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------