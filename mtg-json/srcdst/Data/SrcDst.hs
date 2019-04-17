--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{- | Sources and Destinations.

-}

module Data.SrcDst

  ( SrcDst(..)
  , Src(..)
  , Dst(..)
  , URL(..)

  , prettySrc
  , parseSrc

  , parseDst
  , prettyDst

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Prelude.SrcDst 

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "bytestring" Data.ByteString.Char8            as StrictASCII
import qualified "bytestring" Data.ByteString.Lazy.Char8       as LazyASCII

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | Read the source ('Src'), and write it to a destination ('Dst').

-}

data SrcDst = SrcDst

  { src :: Src
  , dst :: Dst
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{- | A local or remote source of some data.

-}

data Src

  = SrcBytes  LazyBytes
  | SrcBytes' StrictBytes

  | SrcStdin
  | SrcUri   URL
  | SrcFile  FilePath

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseSrc'@
instance IsString Src where fromString = parseSrc

--------------------------------------------------
--------------------------------------------------

{- | A local destination for some data.

-}

data Dst

  = DstStdout
  | DstFile    FilePath

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'parseDst'@
instance IsString Dst where fromString = parseDst

--------------------------------------------------
--------------------------------------------------

{- | 

-}

newtype URL = URL

  String

  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString URL where
  fromString = coerce

--------------------------------------------------
-- Functions: Accessors --------------------------
--------------------------------------------------

-- | `URL` accessor.

fromURL :: URL -> String
fromURL (URL s) = s

--------------------------------------------------
-- Functions: Printing / Parsing -----------------
--------------------------------------------------

{- | 
== Examples

>>> parseSrc "-"
SrcStdin
>>> parseSrc "./mtg.json"
SrcFile "./mtg.json"
>>> parseSrc "          ./mtg.json          "
SrcFile "./mtg.json"

-}

parseSrc :: String -> Src
parseSrc = munge > \case

  "-" -> SrcStdin

  s -> SrcFile s

  where

  munge = lrstrip

--------------------------------------------------

{- | 
== Examples

>>> parseDst "-"
DstStdout
>>> parseDst "./mtg.hs"
DstFile "./mtg.hs"
>>> parseDst "          ./mtg.hs          "
DstFile "./mtg.hs"

-}

parseDst :: String -> Dst
parseDst = munge > \case

  "-" -> DstStdout

  s -> DstFile s

  where

  munge = lrstrip

--------------------------------------------------

prettySrc :: Src -> String
prettySrc = \case

  SrcBytes  bs -> LazyASCII.unpack   bs
  SrcBytes' bs -> StrictASCII.unpack bs

  SrcStdin   -> "<<stdin>>"
  SrcFile fp -> "" <> fp
  SrcUri url -> "" <> fromURL url

--------------------------------------------------

prettyDst :: Dst -> String
prettyDst = \case

  DstStdout  -> "<<stdout>>"
  DstFile fp -> "" <> fp

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- 

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------