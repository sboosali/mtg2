--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{- | XDG-conformant application-specific filepaths.

-}

module Program.MTG.JSON.Paths where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

--import Program.MTG.JSON.Types
import Program.MTG.JSON.Constants
import Program.MTG.JSON.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "time" Data.Time.LocalTime as Time
import qualified "time" Data.Time.Format    as Time

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

-- import qualified "filepath"  System.FilePath  as File
import qualified "directory" System.Directory as Directory

--------------------------------------------------

-- import qualified "base" System.IO as IO
import qualified "base" Data.List as List

-- import qualified "base" Prelude

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

timeFormatWithHyphensAndUnits :: String
timeFormatWithHyphensAndUnits = "%Y-%m-%d-%Hh-%Mm-%Ss-%03qms"

-- NOTE given the meta-syntax « %<modifier><width><alternate><specifier> »,
--      the syntax « %03q » means (0-padded) 3-width picoseconds (i.e. milliseconds).

--------------------------------------------------

replacedCharacters_escapeFilePath :: [Char]
replacedCharacters_escapeFilePath = "/\\ _:;'\"!#$%^&*?|[]{}()"

--------------------------------------------------

replacementCharacter_escapeFilePath :: Char
replacementCharacter_escapeFilePath = '_'

--------------------------------------------------

default_escapeFilePath :: FilePath
default_escapeFilePath = "file"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | Application-specific filepath to a temporary file.

Creates any missing parent directories.

e.g.:

@
> newTemporaryFilePath "mtg.json.gz"
"/tmp/mtg-json/2019-04-06-21h-43m-51s-852ms_mtg.json.gz"
@

-}

newTemporaryFilePath :: String -> IO FilePath
newTemporaryFilePath name' = do

  directory <- Directory.getTemporaryDirectory
  time      <- Time.getZonedTime

  let timestamp = formatZonedTimeAsFilePath time

  let dirname  = directory </> programExecutable
  let basename = timestamp <> "_" <> name 

  mkdir_p dirname

  let path = dirname </> basename

  return path

  where

  name = escapeFilePath name'

--------------------------------------------------

{- | Escape the given string as both a /valid/ and /idiomatic/ filepath.

== Examples

>>> escapeFilePath "https://mtgjson.com/json/Vintage.json.gz"
"https_mtgjson.com_json_Vintage.json.gz"

>>> escapeFilePath ""
"file"

-}

escapeFilePath :: String -> FilePath
escapeFilePath s = fp
  where

  fp :: FilePath
  fp = case s of

    "" -> default_escapeFilePath
    _  -> go s

  go
    = replaceSubstring replacedCharacters_escapeFilePath [replacementCharacter_escapeFilePath]
    > List.group
    > fmap collapseReplacementCharacters
    > concat

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

-}

formatZonedTimeAsFilePath :: Time.ZonedTime -> String
formatZonedTimeAsFilePath t =

  Time.formatTime locale timeFormatWithHyphensAndUnits t

  where

  locale = Time.defaultTimeLocale
  -- NOTE even « Prelude.undefined » works as the locale for « ZonedTime » (it's ignored).

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- 

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------