{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

--------------------------------------------------
--------------------------------------------------

{-| Each set is represented as a JSON object (see 'SetObject').

== Schemata

Each /set/ in @AllSets.json@ looks like:

@
{

  "baseSetSize": ...,
  "block": ...,
  "boosterV3": ...,
  "cards": ...,
  "code": ...,
  "isFoilOnly": ...,
  "isOnlineOnly": ...,
  "keyruneCode": ...,
  "mcmId": ...,
  "mcmName": ...,
  "meta": ...,
  "name": ...,
  "releaseDate": ...,
  "tcgplayerGroupId": ...,
  "tokens": ...,
  "totalSetSize": ...,
  "translations": ...,
  "type": ...,

}
@

from which we keep these fields:

* @baseSetSize@
* @block@
* @cards@
* @code@
* @isFoilOnly@
* @isOnlineOnly@
* @keyruneCode@
* @mcmId@
* @mcmName@
* @meta@
* @name@
* @releaseDate@
* @tcgplayerGroupId@
* @tokens@
* @totalSetSize@
* @translations@
* @type@

== Examples

/Limited Edition Alpha/ from @Vintage.json@:

@
{
    "LEA": {
        "baseSetSize": 302,
        "block": "Core Set",
        "boosterV3": [
            "rare",
            "uncommon",
            "uncommon",
            "uncommon",
            "common",
            "common",
            "common",
            "common",
            "common",
            "common",
            "common",
            "common",
            "common",
            "common",
            "common"
        ],
        "cards": [
            ...
        ],
        "code": "LEA",
        "isFoilOnly": false,
        "isOnlineOnly": false,
        "keyruneCode": "LEA",
        "mcmId": 1,
        "mcmName": "Alpha",
        "meta": {
            "date": "2019-03-31",
            "version": "4.3.2"
        },
        "name": "Limited Edition Alpha",
        "releaseDate": "1993-08-05",
        "tcgplayerGroupId": 7,
        "tokens": [],
        "totalSetSize": 295,
        "translations": {},
        "type": "core"
    }
}
@

-}

module MTG.JSON.Schema.Set where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "base" Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------


{- | @JSON Object@ which represents a "Set" (of `CardObject`s).

== Fields

== Links

* schema — <https://mtgjson.com/structures/set/>

-}



--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------