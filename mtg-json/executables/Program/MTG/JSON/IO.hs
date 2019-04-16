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

  mtg_hs <- parseJSON

  putStdErr sDst

  outputDst

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

  inputSrc :: IO ()
  inputSrc = case src of

      SrcStdin -> do

          promptSrc

      SrcFile fp -> do

          readSrc fp

      SrcUri uri -> do

          fetchSrc uri

  ------------------------------

  outputDst :: IO ()
  outputDst = case dst of

      DstStdout -> do

          printDst

      DstFile fp -> do

          writeDst fp

  ------------------------------

  promptSrc :: IO String
  promptSrc = _

  ------------------------------

  readSrc :: FilePath -> IO String
  readSrc fp = IO.readFile fp

  ------------------------------

  fetchSrc :: URI -> IO String
  fetchSrc uri = IO.readFile uri  -- TODO -- download, decompress, read.

  ------------------------------

  writeDst :: FilePath -> String -> IO ()
  writeDst fp mtg_hs = case force of

      RespectExisting -> do

        let e = runFormat ("\n[Error] Filepath {{{ " % Format.string % " }}} already exists. Rerun <<< mtg-json >>> with the <<< --force >>> option to overwrite.\n") fp -- TODO -- prompt user to confirm.

        if   Directory.doesPathExist fp
        then putStdErr e
        else IO.writeFile fp mtg_hs

      OverwriteExisting -> do

        IO.writeFile fp mtg_hs

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

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------]