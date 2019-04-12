--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-|

-}
module MTG.Enum.Legality where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

newtype Legality = Legality Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @= 'Legal'@
instance Default Legality where def = Legal

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

pattern Legal :: Legality
pattern Legal = "legal"

pattern Restricted :: Legality
pattern Restricted = "restricted"

pattern Banned :: Legality
pattern Banned = "banned"

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Legality

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------