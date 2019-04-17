--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE NamedFieldPuns        #-}

--------------------------------------------------

{- | Sources and Destinations.

-}

module Program.MTG.JSON.SrcDst

  ( SrcDst(..)
  , Src(..)
  , Dst(..)
  , URL(..)

  , readSrc
  , copySrc

  -- , writeDst
  -- , copyDst

  , prettySrc
  , parseSrc

  , parseDst
  , prettyDst

  ) where

--------------------------------------------------
-- Imports (Internal) ----------------------------
--------------------------------------------------

import Program.MTG.JSON.Prelude

--------------------------------------------------
-- Imports (External) ----------------------------
--------------------------------------------------

import qualified "conduit" Conduit as Conduit
import           "conduit" Conduit ( ConduitM )

--------------------------------------------------
--- Imports --------------------------------------
--------------------------------------------------

import qualified "directory" System.Directory as Directory

--------------------------------------------------

import qualified "bytestring" Data.ByteString            as Strict
import qualified "bytestring" Data.ByteString.Lazy       as Lazy

--------------------------------------------------

import qualified "base" System.IO as IO

import qualified "base" Prelude

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
-- Functions -------------------------------------
--------------------------------------------------

{- | Create a @conduit@ /Source/, from the source `Src`.

-}

conduitSrc
  :: ( MonadIO m )
  => Src
  -> ConduitM () o m r

conduitSrc = \case

  SrcBytes  bs -> bs
  SrcBytes' bs -> bs

  SrcStdin   -> _
  SrcFile fp -> fp
  SrcUri uri -> uri

--------------------------------------------------

{- | Create a @conduit@ /Sink/, into the destination `Dst`.

-}

conduitDst
  :: ( MonadIO m )
  => Dst
  -> ConduitM i Void m r

conduitDst = \case

  DstStdout  -> _
  DstFile fp -> fp

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | Read bytes “lazily” (`LazyBytes`) from a given source (`Src`).

-}

readSrc :: Src -> IO LazyBytes
readSrc = \case

  SrcBytes  bs -> return bs
  SrcBytes' bs -> return (Lazy.fromChunks [bs])

  SrcStdin   -> readSrcStdin
  SrcFile fp -> Lazy.readFile fp
  SrcUri uri -> do

    fp <- newTemporaryFilePath uri

    download Nothing uri fp
    Lazy.readFile fp

--------------------------------------------------

{- | Copy the given source (`Src`) to a file (`FilePath`).

-}

copySrc :: FilePath -> Src -> IO LazyBytes
copySrc fpDst = \case

  SrcBytes  bsSrc -> Lazy.writeFile fpDst bsSrc
  SrcBytes' bsSrc -> Strict.writeFile fpDst bsSrc

  SrcStdin      -> readSrcStdin >>= Lazy.writeFile fpDst
  SrcFile fpSrc -> Directory.copyFile fpSrc fpDst
  SrcUri uriSrc -> download Nothing uriSrc fpDst

--------------------------------------------------
-- Functions -------------------------------------
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

  SrcStdin   -> "<<stdin>>"
  SrcFile fp -> "" <> fp
  SrcUri uri -> "" <> uri

--------------------------------------------------

prettyDst :: Dst -> String
prettyDst = \case

  DstStdout  -> "<<stdout>>"
  DstFile fp -> "" <> fp

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

{- | Read bytes “lazily” (`LazyBytes`) from @stdin@ (`IO.stdin`).

-}

readSrcStdin :: IO LazyBytes
readSrcStdin = do
  IO.hSetBinaryMode IO.stdin True
  Lazy.hGetContents IO.stdin

--------------------------------------------------

{- | Download a file.

@download method uri fp@ downloads URI @uri@ to FilePath @fp@ (with optional request method @method@).

The file contents being downloaded may be larger than available memory. @uri@ is streamed into @fp@.

== Examples

@
>> download (Just "GET") "https://mtgjson.com/json/Vintage.json.gz" "/tmp/Vintage.json.gz"
@

-}

download :: Maybe String -> URI -> FilePath -> IO ()
download method uri fp = do

  request <- HTTP.parseRequest uri'

  Resource.runResourceT (HTTP.Conduit.httpSink request consume)

  where

  method' = method & maybe "" (++ " ")
  uri'    = method' <> uri

  consume :: HTTP.Response () -> ConduitM ByteString Void (ResourceT IO) ()
  consume response = do

    Conduit.sinkFileCautious fp

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

-- data ConduitT i o m r

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------