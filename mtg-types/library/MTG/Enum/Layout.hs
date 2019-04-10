{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|



-}
module MTG.Enum.Layout where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Layout = Layout Text
 
  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Layout

-- | @= 'normalLayout'@
instance Default Layout where def = normalLayout

--------------------------------------------------

toLayout :: Maybe Text -> Layout
toLayout = maybe def Layout

--------------------------------------------------

{-|
-}
aftermathLayout :: Layout
aftermathLayout = "aftermath"

{-|
-}
doubleFacedLayout :: Layout
doubleFacedLayout = "double-faced"

{-|
-}
flipLayout :: Layout
flipLayout = "flip"

{-|
-}
levelerLayout :: Layout
levelerLayout = "leveler"

{-|
-}
meldLayout :: Layout
meldLayout = "meld"

{-|
-}
normalLayout :: Layout
normalLayout = "normal"

{-|
-}
planeLayout :: Layout
planeLayout = "plane"

{-|
-}
phenomenonLayout :: Layout
phenomenonLayout = "phenomenon"

{-|
-}
schemeLayout :: Layout
schemeLayout = "scheme"

{-|
-}
splitLayout :: Layout
splitLayout = "split"

{-|
-}
tokenLayout :: Layout
tokenLayout = "token"

{-|
-}
vanguardLayout :: Layout
vanguardLayout = "vanguard"

--------------------------------------------------

{-

-}

