--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE PackageImports #-}

--------------------------------------------------

{- | Re-export:

* the "Prelude.Spiros" @module@ — my custom prelude (from the @spiros@ package).

-}

module Prelude.SrcDst

  ( module EXPORT
  , module Prelude.SrcDst
  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros as EXPORT

--------------------------------------------------

import "filepath" System.FilePath as EXPORT ( (</>), (<.>) )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "time" Data.Time.LocalTime as Time
import qualified "time" Data.Time.Format    as Time

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting (Format)

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "containers" Data.Map as Map
import qualified "containers" Data.Set as Set

--------------------------------------------------

import qualified "base" Data.List as List

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

-- | @mkdir -p@

mkdir_p :: FilePath -> IO ()
mkdir_p = Directory.createDirectoryIfMissing True

--------------------------------------------------

invertMap :: (Ord k, Ord v) => Map k v -> Map v (NonEmpty k)
invertMap kvs = vkss
  where

  vkss = go kvs

  go
    = Map.toList
    > fmap (\(k, v) -> (v, k:|[]))
    > Map.fromListWith (<>)

--------------------------------------------------

{- | Run a format-specification.

== Implementation

Wraps `Format.formatToString`

-}

runFormat :: Format String a -> a
runFormat = Format.formatToString

{-# INLINEABLE runFormat #-}

--------------------------------------------------
-- Constants: Time -------------------------------
--------------------------------------------------

timeFormatWithHyphensAndUnits :: String
timeFormatWithHyphensAndUnits = "%Y-%m-%d-%Hh-%Mm-%Ss-%03qms"

-- NOTE given the meta-syntax « %<modifier><width><alternate><specifier> »,
--      the syntax « %03q » means (0-padded) 3-width picoseconds (i.e. milliseconds).

--------------------------------------------------

replacedCharacters_escapeFilePath :: Set Char
replacedCharacters_escapeFilePath = Set.fromList "/\\ _:;'\"!#$%^&*?|[]{}()"

--------------------------------------------------

replacementCharacter_escapeFilePath :: Char
replacementCharacter_escapeFilePath = '_'

--------------------------------------------------

default_escapeFilePath :: FilePath
default_escapeFilePath = "file"

--------------------------------------------------
-- Functions: Time -------------------------------
--------------------------------------------------

{- | Application-specific filepath to a temporary file.

Creates any missing parent directories.

e.g.:

@
> newTemporaryFilePath (Just "mtg-json") "mtg.json.gz"
"/tmp/mtg-json/2019-04-06-21h-43m-51s-852ms_mtg.json.gz"

> newTemporaryFilePath Nothing ""
"/tmp/2019-04-06-21h-43m-51s-852ms_file"
@

-}

newTemporaryFilePath
  :: Maybe String
  -- ^ Optional directory name (for “namespacing”).
  -> String
  -- ^ File name (suffix).
  -> IO FilePath
  -- ^ Generated filepath.

newTemporaryFilePath directoryName fileName = do

  directory <- Directory.getTemporaryDirectory
  time      <- Time.getZonedTime

  let timestamp = formatZonedTimeAsFilePath time

  let dirname  = (maybe directory (directory </>)) dName
  let basename = timestamp <> "_" <> fName 

  mkdir_p dirname

  let path = dirname </> basename

  return path

  where

  fName = escapeFilePath fileName
  dName = escapeFilePath <$> directoryName

--------------------------------------------------

{- | Escape the given string as both a /valid/ and /idiomatic/ filepath.

== Examples

>>> escapeFilePath ""
"file"

>>> escapeFilePath "https://mtgjson.com/json/Vintage.json.gz"
"https_mtgjson.com_json_Vintage.json.gz"

-}

escapeFilePath :: String -> FilePath
escapeFilePath s = fp
  where

  fp :: FilePath
  fp = case s of

    "" -> default_escapeFilePath
    _  -> go s

  go
    = applyReplacements
    > List.group
    > fmap collapseReplacementCharacters
    > concat

  applyReplacements :: String -> String
  applyReplacements = fmap applyReplacement

  applyReplacement :: Char -> Char
  applyReplacement c =
    if   c `Set.member` replacedCharacters_escapeFilePath
    then replacementCharacter_escapeFilePath
    else c

  collapseReplacementCharacters :: String -> String
  collapseReplacementCharacters t =

    if   List.all (== replacementCharacter_escapeFilePath) t
    then [replacementCharacter_escapeFilePath]
    else t

--------------------------------------------------

{- | Format a timestamp to be part of a filepath.

e.g.:

@
> formatZonedTimeAsFilePath _
"2019-04-06-21h-43m-51s-852ms"
@

== Examples

>>> import Data.Time.LocalTime ( ZonedTime(..), TimeZone(..), LocalTime(..), TimeOfDay(..) )
>>> import Data.Time.Calendar ( Day(..), fromGregorian )
>>> zone = TimeZone { timeZoneMinutes = -420, timeZoneSummerOnly = True, timeZoneName = "PDT" }
>>> time = LocalTime { localDay = fromGregorian 1991 02 28, localTimeOfDay = TimeOfDay { todHour = 13, todMin = 37, todSec = 59 }}
>>> time' = ZonedTime{ zonedTimeToLocalTime = time, zonedTimeZone = zone }
>>> formatZonedTimeAsFilePath time'
"1991-02-28-13h-37m-59s-000ms"

-}

formatZonedTimeAsFilePath :: Time.ZonedTime -> String
formatZonedTimeAsFilePath t =

  Time.formatTime locale timeFormatWithHyphensAndUnits t

  where

  locale = Time.defaultTimeLocale

  -- NOTE even « Prelude.undefined » works as the locale for « ZonedTime » (it's ignored).

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------