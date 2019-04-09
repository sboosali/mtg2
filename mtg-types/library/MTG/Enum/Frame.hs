{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTG.Enum.Frame where

import MTG.Prelude

import Control.Lens (makePrisms)

--------------------------------------------------

newtype Frame = Frame Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

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
