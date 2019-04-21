--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

--------------------------------------------------

{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE PatternSynonyms       #-}

--------------------------------------------------

{- | The *typeline* along the middle of a /Magic: The Gathering/ card.

== Examples

-}

module MTG.Struct.Typeline where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Text.Supertype
import MTG.Text.Cardtype
import MTG.Text.Subtype

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makeLenses)

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ((%))

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "base" Control.Exception as E
import           "base" Control.Exception (PatternMatchFail(..))

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{- | 

-}

data Typeline = Typeline

  { _subtypes   :: [Subtype]     -- TODO `Set`
  , _cardtypes  :: [Cardtype]    -- TODO `NonEmptySet`
  , _supertypes :: [Supertype]   -- TODO `Set`
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Data)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @≡ 'emptyTypeline'@

instance Default Typeline where def = emptyTypeline

--------------------------------------------------

{- | All fields are empty (i.e. @*types = []@).

/NOTE/ this isn't a valid typeline.
Each card should have at least one card type.

-}

emptyTypeline :: Typeline
emptyTypeline = Typeline{..}
  where

  _subtypes   = []
  _cardtypes  = []
  _supertypes = []

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makeLenses ''Typeline

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------