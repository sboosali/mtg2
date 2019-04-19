--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{-| Command-Line Interface.

-}

module Program.MTG.JSON.CLI where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.MTG.JSON.Types
import Program.MTG.JSON.Constants
import Program.MTG.JSON.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "optparse-applicative" Options.Applicative      as P
import qualified "optparse-applicative" Options.Applicative.Help as P hiding (fullDesc)

-- NOTE « attoparsec » uses strict « Text ».

import qualified "optparse-applicative" Options.Applicative.Help.Pretty as PP

-- NOTE « optparse-applicative » re-exports « Text.PrettyPrint.ANSI.Leijen »
--      from the « ansi-wl-pprint » package (via the « ansi-pretty » package).

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ( (%) )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "base" Prelude

--------------------------------------------------
-- CLI -------------------------------------------
--------------------------------------------------

getCommand :: IO Command
getCommand = do

  P.customExecParser preferences piCommand

{-# INLINEABLE getCommand #-}

--------------------------------------------------

parseCommand :: (MonadThrow m) => [String] -> m Command
parseCommand

  = P.execParserPure preferences piCommand
  > fromParserResult
  > either throwM return

{-# INLINEABLE parseCommand #-}

--------------------------------------------------

{-|

-}

preferences :: P.ParserPrefs
preferences = P.prefs (mconcat xs)
  where

  xs =
    [ P.showHelpOnError
    , P.showHelpOnEmpty
    ]

{-# INLINEABLE preferences #-}

--------------------------------------------------

programDescription :: [String] -- [PP.Doc]
programDescription =

  [ "{{{ mtg-json }}} is a program for fetching and extending a list of {{{ Magic: The Gathering }} cards."
  ]

{-# INLINEABLE programDescription #-}

--------------------------------------------------
-- « ParserInfo »s -------------------------------
--------------------------------------------------

-- | 

piCommand :: P.ParserInfo Command
piCommand = info header description pCommand
  where

  header :: String
  header = ""

  description :: String
  description = Prelude.unlines programDescription

{-# INLINEABLE piCommand #-}

--------------------------------------------------

-- | 

piFetch :: P.ParserInfo Subcommand
piFetch = info header description pFetch
  where

  header :: String
  header = ""

  description :: String
  description = "Fetch {{{ mtg.json }}} from SRC, save it in DST."

{-# INLINEABLE piFetch #-}

--------------------------------------------------

-- | 

piPrint :: P.ParserInfo Subcommand
piPrint = info header description pPrint
  where

  header :: String
  header = ""

  description :: String
  description = "Print out the program version or license, or its current configuration."

{-# INLINEABLE piPrint #-}

--------------------------------------------------
-- « Parser »s -----------------------------------
--------------------------------------------------

{- | Command-line (sub'Command's and 'Option's).

@mtg-json@'s @main@ parser.

-}

pCommand :: P.Parser Command
pCommand = do

  options    <- pOptions
  subcommand <- pSubcommand

  return Command{options,subcommand}

{-# INLINEABLE pCommand #-}

--------------------------------------------------

{- | @mtg-json@'s (global) options. -}

pOptions :: P.Parser Options
pOptions = do

  ------------------------------

  verbose <- (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , P.style   P.bold
        , P.help    "Enable verbose messages. (Includes network progress from downloading any resources. Includes printing the config that's derived from the invocation of this command: (1), parsing these command-line options; and (2), defaulting the values of any optional options.). {{{ -v }}} abbreviates \"verbose\"."
        ])

  ------------------------------

  dryrun <- (P.flag TrueRun DryRun) (mconcat

        [ P.long    "dryrun"
        , P.short   'z'
        , P.style   P.bold
        , P.help    "Disable effects. Whether the execution will just be a 'dry-run' (i.e. most effects are disabled, instead they are printed out). {{{ -z }}} abbreviates \"zero effects\"."
        ])

  ------------------------------

  forcefulness <- (P.flag RespectExisting OverwriteExisting) (mconcat

        [ P.long    "force"
        , P.short   'f'
        , P.style   P.bold
        , P.help    "Overwrite FILE. Whether existing files will be overwritten, or preserved (prompting for confirmation). {{{ -f }}} abbreviates \"forcefully overwrite\"."
        ])

  ------------------------------

  return Options{..}

{-# INLINEABLE pOptions #-}

--------------------------------------------------

{- | @mtg-json@'s 'Subcommand's. -}

pSubcommand :: P.Parser Subcommand
pSubcommand = P.hsubparser (mconcat ps)
  where

  ps :: [P.Mod P.CommandFields Subcommand]
  ps =
      [ P.command "fetch" piFetch
      , P.command "print" piPrint
      ]

{-# INLINEABLE pSubcommand #-}

--------------------------------------------------

-- | 

pFetch :: P.Parser Subcommand
pFetch = Fetch <$> do

  src <- pSrc
  dst <- pDst

  return SrcDst{ src, dst }

  where

  ------------------------------

  pSrc :: P.Parser Src
  pSrc = (P.option (parseSrc <$> P.str)) (mconcat

        [ P.long    "input"
        , P.short   'i'
        , P.metavar "URI"
        , P.value   defaultSrc
        , P.showDefault
        , P.completeWith cSrc
        , P.style   P.bold
        , P.help    "Where to input the {{{ mtg.json }}} JSON file from."
        ])

  pDst :: P.Parser Dst
  pDst = (P.option (parseDst <$> P.str)) (mconcat

        [ P.long    "output"
        , P.short   'o'
        , P.metavar "FILE"
        , P.value   defaultDst
        , P.showDefault
        , P.action  "file"
        , P.style   P.bold
        , P.help    "Where to output the parsed {{{ mtg.hs }}} Haskell file into (readable via the {{{ Read }}} typeclass)."
        ])

  ------------------------------

  defaultSrc :: Src
  defaultSrc = SrcUri defaultSource

  defaultDst :: Dst
  defaultDst = DstFile defaultDestination

  ------------------------------

  -- rSrc :: P.ReadM Src
  -- cSrc :: [String]

  (_rSrc, cSrc) = (pAssoc (bimap id SrcUri <$> knownSources))

  ------------------------------

{-# INLINEABLE pFetch #-}

--TODO-- P.helpDoc :: Maybe Doc -> Mod f a

--------------------------------------------------

-- | 

pPrint :: P.Parser Subcommand
pPrint = P.argument rPrint fields
  where

  fields :: P.Mod P.ArgumentFields a
  fields = mconcat
    [ P.completeWith cPrint
    ]

  rPrint :: P.ReadM Subcommand
  cPrint :: [String]
  (rPrint, cPrint) = pAssoc cs

  cs :: [(String, Subcommand)]
  cs =
    [ "version"-: PrintVersion
    , "license"-: PrintLicense
    ]

{-# INLINEABLE pPrint #-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

{-| -}

defaulting :: (Alternative f) => a -> f a -> f a
defaulting x = \p -> maybe x id <$> optional p

{-# INLINEABLE defaulting #-}

--------------------------------------------------

{-| -}

info
  :: forall a. String
       -> String
       -> P.Parser a
       -> P.ParserInfo a

info header description p = pi
  where

  pi = P.info (P.helper <*> p) information

  information :: P.InfoMod a
  information = mconcat

      [ P.fullDesc
      , P.progDesc description
      , P.header   header
      ]

{-# INLINEABLE info #-}

--------------------------------------------------

{-| -}

fromParserResult :: (HasCallStack) => P.ParserResult a -> Either SomeException a
fromParserResult = \case

    P.Success a           -> Right a
    P.Failure e           -> Left (toStdErrException (P.renderFailure e programExecutable))
    P.CompletionInvoked _ -> Left (toStdErrException (programExecutable, ExitFailure 1))

  where

    toStdErrException :: (String, ExitCode) -> SomeException
    toStdErrException (stderr, exitcode) = toException (errorCall s)
      where

        s :: String
        s = runFormat ("Exit Code: " % Format.int % "\nStd Err:\n" % Format.string)
            (fromExitCode exitcode)
            stderr

{-# INLINEABLE fromParserResult #-}

--------------------------------------------------

{-| -}

docsToChunk :: [PP.Doc] -> P.Chunk PP.Doc
docsToChunk = PP.vcat > Just > P.Chunk

{-# INLINEABLE docsToChunk #-}

--------------------------------------------------

{-| -}

helpDocs :: [PP.Doc] -> P.Mod f a
helpDocs docs = P.helpDoc (Just doc)
  where

  doc :: PP.Doc
  doc = PP.hcat docs

--------------------------------------------------

--TODO-- « terminfo »

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

ansi-wl-pprint(:Text.PrettyPrint.ANSI.Leijen)._

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------