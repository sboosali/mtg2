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

import qualified "containers" Data.Map as Map

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

{- | Multiple `SrcDst`s.

Each destination has (exactly) one source; i.e.
sources shouldn't collide.

== Implementation

`SrcDst` is represented “reversed”, into @( `Dst`, `Src` )@.
This enforces the "unique destination" property.

-}

newtype DstSrcs = DstSrcs

  ( Map Dst Src )

  deriving stock    (Lift,Generic)
  deriving newtype  (Show,Read)
  deriving newtype  (Eq,Ord)
  deriving newtype  (NFData)

--------------------------------------------------

-- | @`fromList` ≡ `toDstSrcs`@
instance IsList DstSrcs where
  type Item DstSrcs = SrcDst
  fromList = toDstSrcs
  toList   = fromDstSrcs

--------------------------------------------------
--------------------------------------------------

{- | 

-}

newtype URL = URL

  { fromURL ::
      String
  }

  deriving stock    (Lift,Data,Generic)
  deriving newtype  (Eq,Ord)
  deriving newtype  (Show,Read) -- NOTE -- hides accessor from printing.
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString URL where
  fromString = coerce

--------------------------------------------------
-- Functions: Conversion -------------------------
--------------------------------------------------

toDstSrcs :: [SrcDst] -> DstSrcs
toDstSrcs srcdsts = DstSrcs (Map.fromList attributes)
  where

  attributes :: [( Dst, Src )]
  attributes = switch <$> srcdsts

  switch SrcDst{ src, dst } = ( dst, src )

--------------------------------------------------

fromDstSrcs :: DstSrcs -> [SrcDst]
fromDstSrcs (DstSrcs kvs) = Map.toList kvs <&> switch
  where

  switch ( dst, src ) = SrcDst{ src, dst }

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