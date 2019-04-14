--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-| 'BlockName' is the name of a /Magic: The Gathering/ block.

== Types

* `BlockName`
* `BlockInfo`

-}

module MTG.Text.Block where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Text.Edition

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makeLenses, makePrisms)

--------------------------------------------------

-- import qualified "parsers" Text.Parser.Combinators as P
-- import qualified "parsers" Text.Parser.Char        as P

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

newtype BlockName = BlockName

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

data BlockInfo = BlockInfo

  { _blockAbbreviation :: Text
  , _blockDescription  :: Text
  , _blockBlocks        :: [EditionName]
--, _blockLanguages    :: [Language] --NOTE a `Set`
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)

  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

pattern AntediluvianPseudoBlock :: BlockName
pattern AntediluvianPseudoBlock = BlockName "Antediluvian Sets"

pattern OrdinalPseudoBlock :: BlockName
pattern OrdinalPseudoBlock = BlockName "Ordinal Core Sets"

pattern CardinalPseudoBlock :: BlockName
pattern CardinalPseudoBlock = BlockName "Cardinal Core Sets"

--------------------------------------------------

pattern MirageBlock :: BlockName
pattern MirageBlock = BlockName "Mirage"

pattern RathBlock :: BlockName
pattern RathBlock = BlockName "Rath"

pattern UrzaBlock :: BlockName
pattern UrzaBlock = BlockName "Urza"

pattern MasquesBlock :: BlockName
pattern MasquesBlock = BlockName "Masques"

pattern InvasionBlock :: BlockName
pattern InvasionBlock = BlockName "Invasion"

pattern OdysseyBlock :: BlockName
pattern OdysseyBlock = BlockName "Odyssey"

pattern OnslaughtBlock :: BlockName
pattern OnslaughtBlock = BlockName "Onslaught"

pattern MirrodinBlock :: BlockName
pattern MirrodinBlock = BlockName "Mirrodin"

pattern KamigawaBlock :: BlockName
pattern KamigawaBlock = BlockName "Kamigawa"

pattern RavnicaBlock :: BlockName
pattern RavnicaBlock = BlockName "Ravnica"

pattern IceageBlock :: BlockName
pattern IceageBlock = BlockName "Ice Age"

pattern TimespiralBlock :: BlockName
pattern TimespiralBlock = BlockName "Time Spiral"

pattern LorwynBlock :: BlockName
pattern LorwynBlock = BlockName "Lorwyn"

pattern ShadowmoorBlock :: BlockName
pattern ShadowmoorBlock = BlockName "Shadowmoor"

pattern AlaraBlock :: BlockName
pattern AlaraBlock = BlockName "Shards Of Alara"

pattern ZendikarBlock :: BlockName
pattern ZendikarBlock = BlockName "Zendikar"

pattern ScarsBlock :: BlockName
pattern ScarsBlock = BlockName "Scars Of Mirrodin"

pattern InnistradBlock :: BlockName
pattern InnistradBlock = BlockName "Innistrad"

pattern RavnicaBlock2 :: BlockName
pattern RavnicaBlock2 = BlockName "Return To Ravnica"

pattern TherosBlock :: BlockName
pattern TherosBlock = BlockName "Theros"

pattern KhansBlock :: BlockName
pattern KhansBlock = BlockName "Khans Of Tarkir"

pattern ZendikarBlock2 :: BlockName
pattern ZendikarBlock2 = BlockName "Battle For Zendikar"

pattern ShadowsBlock :: BlockName
pattern ShadowsBlock = BlockName "Shadows Over Innistrad"

pattern KaladeshBlock :: BlockName
pattern KaladeshBlock = BlockName "Kaladesh"

pattern AmonkhetBlock :: BlockName
pattern AmonkhetBlock = BlockName "Amonkhet"

pattern IxalanBlock :: BlockName
pattern IxalanBlock = BlockName "Ixalan"

pattern RavnicaBlock3 :: BlockName
pattern RavnicaBlock3 = BlockName "Guilds of Ravnica"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

knownBlocks :: [BlockName]
knownBlocks = fakeBlocks ++ realBlocks
  where

  fakeBlocks =

    [ AntediluvianPseudoBlock
    , OrdinalPseudoBlock
    , CardinalPseudoBlock
    ]

  realBlocks =

    [ MirageBlock
    , RathBlock
    , UrzaBlock
    , MasquesBlock
    , InvasionBlock
    , OdysseyBlock
    , OnslaughtBlock
    , MirrodinBlock
    , KamigawaBlock
    , RavnicaBlock
    , IceageBlock
    , TimespiralBlock
    , LorwynBlock
    , ShadowmoorBlock
    , AlaraBlock
    , ZendikarBlock
    , ScarsBlock
    , InnistradBlock
    , RavnicaBlock2
    , TherosBlock
    , KhansBlock
    , ZendikarBlock2
    , ShadowsBlock
    , KaladeshBlock
    , AmonkhetBlock
    , IxalanBlock
    , RavnicaBlock3
    ]

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''BlockName

makeLenses ''BlockInfo

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------