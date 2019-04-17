--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------
{-# LANGUAGE GADTs #-}

--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{-|

-}

module Program.MTG.JSON.Types

  ( module Program.MTG.JSON.Types
  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Data.SrcDst

import Program.MTG.JSON.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "filepath" System.FilePath as File

--------------------------------------------------

import qualified "tar" Codec.Archive.Tar as Tar

--------------------------------------------------

import qualified "zlib" Codec.Compression.GZip as GZip
import qualified "zlib" Codec.Compression.Zlib as Zlib

--------------------------------------------------

import qualified "aeson" Data.Aeson.Types as JSON
import           "aeson" Data.Aeson.Types (FromJSON, ToJSON)

--------------------------------------------------

import qualified "binary" Data.Binary as Binary
import           "binary" Data.Binary (Binary)

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| Represents this program's command-line interface.

-}

data Command = Command

  { subcommand :: Subcommand
  , options    :: Options
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-| Represents this program's subcommands.

-}

data Subcommand

  = PrintVersion
  | PrintLicense

  | FetchJSON SrcDst

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|  Represents this program's global options.

-}

data Options = Options

  { verbose      :: Verbosity
  , dryrun       :: Effectfulness
  , forcefulness :: Forcefulness
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultOptions'@

instance Default Options where def = defaultOptions

--------------------------------------------------

{-|

-}

defaultOptions :: Options
defaultOptions = Options{..}
  where
  verbose      = def
  dryrun       = def
  forcefulness = def

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- |

== CLI

* `Quiet`   — @$ mtg-json -q  ...@ (a.k.a. @--quiet@.)
* `Concise` — @$ mtg-json     ...@ (i.e. the `defaultVerbosity`.)
* `Verbose` — @$ mtg-json -v  ...@ (i.e. one flag.)
* `Loud`    — @$ mtg-json -vv ...@ (i.e. two flags.)

-}

data Verbosity

  = Quiet
  | Concise
  | Verbose
  | Loud

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (GEnum)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultVerbosity'@

instance Default Verbosity where def = defaultVerbosity

--------------------------------------------------

-- | @= 'Concise'@

defaultVerbosity :: Verbosity
defaultVerbosity = Concise

--------------------------------------------------
--------------------------------------------------

{-|
== CLI

* `TrueRun` — @$ mtg-json    ...@ (i.e. the `defaultEffectfulness`.)
* `DryRun`  — @$ mtg-json -z ...@ (a.k.a. @--dryrun@.)

-}

data Effectfulness

  = DryRun
  | TrueRun

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (GEnum)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultEffectfulness'@

instance Default Effectfulness where def = defaultEffectfulness

--------------------------------------------------

-- | @= 'TrueRun'@

defaultEffectfulness :: Effectfulness
defaultEffectfulness = TrueRun

--------------------------------------------------
--------------------------------------------------

{-|
== CLI

* `RespectExisting`   — @$ mtg-json    ...@ (i.e. the `defaultForcefulness`.)
* `OverwriteExisting` — @$ mtg-json -f ...@ (a.k.a. @--force@.)

-}

data Forcefulness

  = RespectExisting
  | OverwriteExisting

  deriving stock    (Enum,Bounded,Ix)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (GEnum)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'defaultForcefulness'@

instance Default Forcefulness where def = defaultForcefulness

--------------------------------------------------

-- | @= 'RespectExisting'@

defaultForcefulness :: Forcefulness
defaultForcefulness = RespectExisting

--------------------------------------------------
--------------------------------------------------
{-TODO-

{-| Whether the program output is colorful or not.

c.f. @grep@:

@
$ grep --color=auto ...
$ grep --color=on   ...
$ grep --color=off  ...
@

-}

data WhetherColorful

  = ColorAuto
  | ColorOn
  | ColorOff

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

-}
--------------------------------------------------
--------------------------------------------------
{-TODO-

{-| Whether the program output has unicode characters or not.

-}

data WhetherUnicode

  = AsciiOnly
  | UnicodeToo

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

-}

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| Represents an @mtg.json@ input (i.e. unparsed).

-}

newtype MTGJSON t = MTGJSON

  t

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | Delegates 'fromString'.

instance (IsString t) => IsString (MTGJSON t) where
  fromString = (MTGJSON . fromString)

--------------------------------------------------
--------------------------------------------------

{-| Represents an @mtg.hs@ output (i.e. parsed).

-}

newtype MTGHS t = MTGHS

  t

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | Delegates 'fromString'.

instance (IsString t) => IsString (MTGHS t) where
  fromString = (MTGHS . fromString)

--------------------------------------------------
-- GADTs -----------------------------------------
--------------------------------------------------

{- | How to @"Program.MTG.JSON.IO.fetch"@ (and process) a resource.

-}

data FetchConfig (a :: *) where

  FetchMtgJsonGz :: URL -> FetchConfig (MTGJSON ByteString)

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------