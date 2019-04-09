{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------
--------------------------------------------------

{-| Each card is represented as a JSON object (see 'CardObject').

== Schemata

Each /card/ in @AllSets.json@ looks like:

@
{

    "artist": "...",
    "borderColor": "black",
    "colorIdentity": [ ... ],
    "colors": [ ... ],
    "convertedManaCost": 0,
    "foreignData": [ ... ],
    "frameVersion": YYYY,
    "hasFoil": false,
    "hasNonFoil": true,
    "isReserved": true,
    "layout": "normal",
    "legalities": { "vintage": "...", ... },
    "manaCost": "{_}...",
    "multiverseId": 0,
    "name": "...",
    "number": "...",
    "originalText": "...",
    "originalType": "...",
    "printings": [ "LEB", ... ],
    "rarity": "...",
    "rulings": [ ... ],
    "scryfallId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
    "scryfallIllustrationId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
    "scryfallOracleId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
    "subtypes": [ ... ],
    "supertypes": [ ... ],
    "text": "...",
    "type": "...",
    "types": [ "Instant", ... ],
    "uuid": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
    "uuidV421": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",

    ...
}
@

from which we keep these fields:

* @artist@
* @borderColor@
* @colorIdentity@
* @colors@
* @convertedManaCost@
* @foreignData@
* @frameVersion@
* @layout@
* @legalities@
* @manaCost@
* @multiverseId@
* @name@
* @number@
* @originalText@
* @originalType@
* @printings@
* @rarity@
* @rulings@
* @scryfallId@
* @scryfallIllustrationId@
* @scryfallOracleId@
* @subtypes@
* @supertypes@
* @text@
* @type@
* @types@
* @uuid@

== Examples

e.g. a spell, @Ancestral Recall@:

@
{

    "artist": "Mark Poole",
    "borderColor": "black",
    "colorIdentity": [
        "U"
    ],
    "colors": [
        "U"
    ],
    "convertedManaCost": 1,
    "foreignData": [],
    "frameVersion": "1993",
    "hasFoil": false,
    "hasNonFoil": true,
    "isReserved": true,
    "layout": "normal",
    "legalities": {
        "commander": "Banned",
        "duel": "Banned",
        "legacy": "Banned",
        "oldschool": "Restricted",
        "vintage": "Restricted"
    },
    "manaCost": "{U}",
    "multiverseId": 95,
    "name": "Ancestral Recall",
    "number": "47",
    "originalText": "Draw 3 cards or force opponent to draw 3 cards.",
    "originalType": "Instant",
    "printings": [
        "2ED",
        "CED",
        "CEI",
        "LEA",
        "LEB",
        "OVNT",
        "PRM",
        "VMA"
    ],
    "rarity": "rare",
    "rulings": [],
    "scryfallId": "70e7ddf2-5604-41e7-bb9d-ddd03d3e9d0b",
    "scryfallIllustrationId": "d20eda7b-a902-4c00-bdab-601059e417b5",
    "scryfallOracleId": "550c74d4-1fcb-406a-b02a-639a760a4380",
    "subtypes": [],
    "supertypes": [],
    "tcgplayerProductId": 1026,
    "tcgplayerPurchaseUrl": "https://mtgjson.com/links/582c0168ba17eac4",
    "text": "Target player draws three cards.",
    "type": "Instant",
    "types": [
        "Instant"
    ],
    "uuid": "3b591d07-f76b-538e-8409-6f35226d3fcd",
    "uuidV421": "abeee6a6-9235-5f87-ad31-23b137d8c1a4"

}
@

e.g. a creature, @Zombie Master@:

@
{

    "artist": "Jeff A. Menges",
    "borderColor": "black",
    "colorIdentity": [
        "B"
    ],
    "colors": [
        "B"
    ],
    "convertedManaCost": 3,
    "flavorText": "They say the Zombie Master controlled these foul creatures even before his own death, but now that he is one of them, nothing can make them betray him.",
    "foreignData": [],
    "frameVersion": "1993",
    "hasFoil": false,
    "hasNonFoil": true,
    "layout": "normal",
    "legalities": {
        "commander": "Legal",
        "duel": "Legal",
        "legacy": "Legal",
        "oldschool": "Legal",
        "vintage": "Legal"
    },
    "manaCost": "{1}{B}{B}",
    "multiverseId": 93,
    "name": "Zombie Master",
    "number": "137",
    "originalText": "All zombies in play gain swampwalk and \"ooB Regenerates\" for as long as this card remains in play.",
    "originalType": "Summon - Lord",
    "power": "2",
    "printings": [
        "2ED",
        "3ED",
        "4ED",
        "5ED",
        "6ED",
        "CED",
        "CEI",
        "FBB",
        "LEA",
        "LEB",
        "ME4",
        "SUM"
    ],
    "rarity": "rare",
    "rulings": [],
    "scryfallId": "3d4255a0-d445-4c00-b936-bbf07851e1c8",
    "scryfallIllustrationId": "7441c48a-ca5a-415c-ad9d-2839e6e47f5b",
    "scryfallOracleId": "5446c92f-ff22-4e9b-a2f6-e64c8560c1e0",
    "subtypes": [
        "Zombie"
    ],
    "supertypes": [],
    "tcgplayerProductId": 1326,
    "tcgplayerPurchaseUrl": "https://mtgjson.com/links/32718321fcedd1bc",
    "text": "Other Zombie creatures have swampwalk. (They can't be blocked as long as defending player controls a Swamp.)\nOther Zombies have \"{B}: Regenerate this permanent.\"",
    "toughness": "3",
    "type": "Creature — Zombie",
    "types": [
        "Creature"
    ],
    "uuid": "ad86cf17-89fc-5171-a8ef-eac823be8660",
    "uuidV421": "32ab9e53-c679-5166-9e0c-fcca266dbbf8"

}
@

-}

module MTG.JSON.Schema.Card where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "base" Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

data CardObject = CardObject 

  {

    "artist": "Jeff A. Menges",
    "borderColor": "black",
    "colorIdentity": [
        "B"
    ],
    "colors": [
        "B"
    ],
    "convertedManaCost": 3,
    "flavorText": "They say the Zombie Master controlled these foul creatures even before his own death, but now that he is one of them, nothing can make them betray him.",
    "foreignData": [],
    "frameVersion": "1993",
    "hasFoil": false,
    "hasNonFoil": true,
    "layout": "normal",
    "legalities": {
        "commander": "Legal",
        "duel": "Legal",
        "legacy": "Legal",
        "oldschool": "Legal",
        "vintage": "Legal"
    },
    "manaCost": "{1}{B}{B}",
    "multiverseId": 93,
    "name": "Zombie Master",
    "number": "137",
    "originalText": "All zombies in play gain swampwalk and \"ooB Regenerates\" for as long as this card remains in play.",
    "originalType": "Summon - Lord",
    "power": "2",
    "printings": [
        "2ED",
        "3ED",
        "4ED",
        "5ED",
        "6ED",
        "CED",
        "CEI",
        "FBB",
        "LEA",
        "LEB",
        "ME4",
        "SUM"
    ],
    "rarity": "rare",
    "rulings": [],
    "scryfallId": "3d4255a0-d445-4c00-b936-bbf07851e1c8",
    "scryfallIllustrationId": "7441c48a-ca5a-415c-ad9d-2839e6e47f5b",
    "scryfallOracleId": "5446c92f-ff22-4e9b-a2f6-e64c8560c1e0",
    "subtypes": [
        "Zombie"
    ],
    "supertypes": [],
    "tcgplayerProductId": 1326,
    "tcgplayerPurchaseUrl": "https://mtgjson.com/links/32718321fcedd1bc",
    "text": "Other Zombie creatures have swampwalk. (They can't be blocked as long as defending player controls a Swamp.)\nOther Zombies have \"{B}: Regenerate this permanent.\"",
    "toughness": "3",
    "type": "Creature — Zombie",
    "types": [
        "Creature"
    ],
    "uuid": "ad86cf17-89fc-5171-a8ef-eac823be8660",
    "uuidV421": "32ab9e53-c679-5166-9e0c-fcca266dbbf8"
 
  { _id            :: Text 
  , _layout        :: Text 
  , _name          :: Text 
  , _names         :: Maybe [Text] 
  , _manaCost      :: Maybe Text 
  , _cmc           :: Natural 
  , _colors        :: Maybe [Text] 
  , _colorIdentity :: Maybe [Text] 
  , _type          :: Text 
  , _supertypes    :: Maybe [Text] 
  , _types         :: Maybe [Text] -- ^ Un-cards can have no type 
  , _subtypes      :: Maybe [Text] 
  , _rarity        :: Text 
  , _text          :: Maybe Text 
  , _flavor        :: Maybe Text 
  , _artist        :: Text
  , _number        :: Maybe Text
  , _power         :: Maybe Text -- ^ Un-cards can have non-integer power/toughness 
  , _toughness     :: Maybe Text  
  , _loyalty       :: Maybe Natural 
  , _multiverseid  :: Maybe Natural
  , _variations    :: Maybe [Natural] 
  , _imageName     :: Maybe Text 
  , _watermark     :: Maybe Text 
  , _border        :: Maybe Text 
  , _timeshifted   :: Maybe Bool -- IsCardTimeShifted
  , _hand          :: Maybe Integer  -- ^ Vanguard only 
  , _life          :: Maybe Integer -- ^ Vanguard only 
  , _reserved      :: Maybe Bool -- IsCardReserved 
  , _releaseDate   :: Maybe Text -- ^ Promo only 
  , _starter       :: Maybe Bool -- IsCardStarter 
  , _mciNumber     :: Maybe Text  -- ^ used by `MagicCards.info`, almost always identical to '_CardObject_number' 
  , _rulings       :: Maybe [CardRulingObject] 
  , _foreignNames  :: Maybe [CardForeignPrintingObject] 
  , _printings     :: [Text]  
  , _originalText  :: Maybe Text 
  , _originalType  :: Maybe Text
  , _legalities    :: Maybe [CardFormatLegalityObject]
  , _source        :: Maybe Text 

  }
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable,Binary)

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| 

-}



--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------