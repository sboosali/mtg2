{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|


-}
module MTG.Enum.Border where

import MTG.Prelude

import Control.Lens (makePrisms)

----------------------------------------

newtype Border = Border Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Border

-- | @= 'blackBorder'@
instance Default Border where def = blackBorder

----------------------------------------

toBorder :: Maybe Text -> Border
toBorder = maybe def Border

----------------------------------------

blackBorder :: Border
blackBorder = "black"

whiteBorder :: Border
whiteBorder = "white"

silverBorder :: Border
silverBorder = "silver"

----------------------------------------
