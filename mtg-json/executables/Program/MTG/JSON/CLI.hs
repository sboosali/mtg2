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

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting (Format)

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import           "base" Data.Maybe
import           "base" System.Exit

--------------------------------------------------

import "base" Prelude

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
-- « ParserInfo »s -------------------------------
--------------------------------------------------

-- | 

piCommand :: P.ParserInfo Command
piCommand = info description pCommand

  where
  description = "{{{ mtg-json }}} is a program for fetching and extending a list of {{{ Magic: The Gathering }} cards."

{-# INLINEABLE piCommand #-}

--------------------------------------------------

-- | 

piFetch :: P.ParserInfo Subcommand
piFetch = info description pFetch

  where
  description = "Fetch {{{ mtg.json }}} from SRC, save it in DST."

{-# INLINEABLE piFetch #-}

--------------------------------------------------

-- | 

piPrint :: P.ParserInfo Subcommand
piPrint = info description pPrint

  where
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

  verbosity <- (P.flag Concise Verbose) (mconcat

        [ P.long    "verbose"
        , P.short   'v'
        , P.style   P.bold
        , P.help    "Enable verbose messages. (Includes network progress from downloading any resources. Includes printing the config that's derived from the invocation of this command: (1), parsing these command-line options; and (2), defaulting the values of any optional options.). {{{ -v }}} abbreviates \"verbose\"."
        ])

  dryrun <- (P.flag TrueRun DryRun) (mconcat

        [ P.long    "dryrun"
        , P.short   'i'
        , P.style   P.bold
        , P.help    "Disable effects. Whether the execution will just be a 'dry-run' (i.e. most effects are disabled, instead they are printed out). {{{ -i }}} abbreviates \"information\"."
        ])

  return Options{..}

{-# INLINEABLE pOptions #-}

--------------------------------------------------

{- | @mtg-json@'s 'Subcommand's. -}

pSubcommand :: P.Parser Subcommand
pSubcommand = P.hsubparser ps
  where

  ps =
      [ P.command "fetch" piFetch
      , P.command "print" piPrint
      ]

{-# INLINEABLE pSubcommand #-}

--------------------------------------------------

-- | 

pFetch :: P.Parser Subcommand
pFetch = do

  src <- pSrc
  dst <- pDst

  let srcdst = SrcDst{ src, dst }

  return (FetchJSON srcdst)

  where

  pSrc :: P.Parser Src
  pSrc = _

  pDst :: P.Parser Dst
  pDst = _

{-# INLINEABLE pFetch #-}

--------------------------------------------------

-- | 

pPrint :: P.Parser Subcommand
pPrint = _

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
  :: forall a.
     String -> P.Parser a
  -> P.ParserInfo a

info description parser = P.info (P.helper <*> parser) information
  where

  information :: P.InfoMod a
  information = mconcat

      [ P.fullDesc
      , P.progDesc description
      ]

{-# INLINEABLE info #-}

--------------------------------------------------

{-| -}

fromParserResult :: (HasCallStack) => P.ParserResult a -> Either SomeException a
fromParserResult = \case

    P.Success a           -> Right a
    P.Failure e           -> Left (toStdErrException (P.renderFailure e programExecutable))
    P.CompletionInvoked _ -> Left def

  where

    toStdErrException :: (String, ExitCode) -> SomeException
    toStdErrException (stderr, exitcode) = toException (errorCall s)
      where

        s :: String
        s = runFormat ("Exit Code: " % Format.int % "\n\
                        Std Err:\n" % Format.string)
            exitcode
            stderr

{-# INLINEABLE fromParserResult #-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------