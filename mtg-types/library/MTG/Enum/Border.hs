{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Border where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Border = Border Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Border

-- | @= 'blackBorder'@
instance Default Border where def = blackBorder

--------------------------------------------------

toBorder :: Maybe Text -> Border
toBorder = maybe def Border

--------------------------------------------------

blackBorder :: Border
blackBorder = "black"

whiteBorder :: Border
whiteBorder = "white"

silverBorder :: Border
silverBorder = "silver"

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------