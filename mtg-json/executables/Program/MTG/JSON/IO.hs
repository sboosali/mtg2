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

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ( Format, (%) )

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

  mtg_hs <- mtgjson2mtghs mtg_json

  putStdErr sDst

  outputDst mtg_hs

  where

  ------------------------------

  sSrc :: String
  sSrc = runFormat ("\nFetching from: " % Format.string % "...\n") s
      where
      s = prettySrc src

  sDst :: String
  sDst = runFormat ("\nSaving to: " % Format.string % "...\n") s
      where
      s = prettyDst src

  ------------------------------

  inputSrc :: IO MTGJSON
  inputSrc = case src of

      SrcStdin -> do

          promptSrc

      SrcLines ts -> do

          MTGJSON (Prelude.unlines ts)

      SrcFile fp -> do

          readSrc fp

      SrcUri uri -> do

          fetchSrc uri

  ------------------------------

  outputDst :: MTGHS -> IO ()
  outputDst mtg_hs = case dst of

      DstStdout -> do

          printDst mtg_hs

      DstFile fp -> do

          writeDst fp mtg_hs

  ------------------------------

  promptSrc :: IO MTGJSON
  promptSrc = MTGJSON <$> do

    IO.hGetContents IO.stdin

    -- IO.hGetContents IO.stdin

  ------------------------------

  readSrc :: FilePath -> IO MTGJSON
  readSrc fp = MTGJSON <$> do

    IO.readFile fp

  ------------------------------

  fetchSrc :: URI -> IO MTGJSON
  fetchSrc uri = MTGJSON <$> do

    IO.readFile uri  -- TODO -- download, decompress, read.

  ------------------------------

  printDst :: MTGHS -> IO ()
  printDst (MTGHS mtg) = putStdOut mtg

  ------------------------------

  writeDst :: FilePath -> MTGHS -> IO ()
  writeDst fp (MTGHS mtg) = case force of

      RespectExisting -> do

        let e = runFormat ("\n[Error] Filepath {{{ " % Format.string % " }}} already exists. Rerun <<< mtg-json >>> with the <<< --force >>> option to overwrite.\n") fp -- TODO -- prompt user to confirm.

        if   Directory.doesPathExist fp
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
-- Utilities -------------------------------------
--------------------------------------------------

mtgjson2mtghs :: MTGJSON -> MTGHS
mtgjson2mtghs (MTGJSON s) = (MTGHS s) -- TODO

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------]