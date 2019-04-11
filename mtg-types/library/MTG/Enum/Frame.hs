{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Frame where

import MTG.Types.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Frame = Frame Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

makePrisms ''Frame
--------------------------------------------------

oldFrame :: Frame
oldFrame = "old"

timeshiftedFrame :: Frame
timeshiftedFrame = "timeshifted"

newFrame :: Frame
newFrame = "new"

futureFrame :: Frame
futureFrame = "future"

--------------------------------------------------
