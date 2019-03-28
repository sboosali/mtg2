
--------------------------------------------------
--------------------------------------------------

{-| 

-}

module MTG.Scryfall.Schemas.Types

  ( Identifier(..)
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

import Prelude.MTG.Scryfall

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-|

e.g.

@
"73542493-cd0b-4bb7-a5b8-8f889c76e4d6" :: 'Identifier'
@

-}

newtype Identifier = Identifier

  Text

  deriving stock    (Show,Read,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Identifier where
  fromString = (coerce . fromString)

--------------------------------------------------
--------------------------------------------------

{-|

  "lang"           : "en",

e.g.

@
"" :: ''
@

-}

newtype  = 

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Identifier where
  fromString = coerce

--------------------------------------------------
--------------------------------------------------

{-|

  "layout"         : "normal",

e.g.

@
"" :: ''
@

-}

newtype  = 

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Identifier where
  fromString = coerce

--------------------------------------------------
--------------------------------------------------

{-|

e.g.

@
"" :: ''
@

-}

newtype  = 

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Identifier where
  fromString = coerce

--------------------------------------------------
--------------------------------------------------

  "mana_cost"      : "{G}",
  "cmc"            : 1,
  "power"          : "1",
  "toughness"      : "1",
  "colors"         : [
    "G"
  ],
  "color_identity" : [
    "G"
  ],

  "legalities" :: Legalities

    "standard"  : "legal",
    "future"    : "legal",
    "frontier"  : "legal",
    "modern"    : "legal",
    "legacy"    : "legal",
    "pauper"    : "legal",
    "vintage"   : "legal",
    "penny"     : "not_legal",
    "commander" : "legal",
    "1v1"       : "legal",
    "duel"      : "legal",
    "brawl"     : "legal"

  "reserved"          : false,
  "foil"              : false,
  "nonfoil"           : true,
  "oversized"         : false,
  "reprint"           : true,
  "set"               : "m19",
  "set_name"          : "Core Set 2019",
  "set_uri"           : "https://api.scryfall.com/sets/m19",
  "set_search_uri"    : "https://api.scryfall.com/cards/search?order=set&q=e%3Am19&unique=prints",
  "scryfall_set_uri"  : "https://scryfall.com/sets/m19?utm_source=api",
  "rulings_uri"       : "https://api.scryfall.com/cards/m19/314/rulings",
  "prints_search_uri" : "https://api.scryfall.com/cards/search?order=set&q=%21%E2%80%9CLlanowar+Elves%E2%80%9D&unique=prints",
  "collector_number"  : "314",
  "digital"           : false,
  "rarity"            : "common",
  "flavor_text"       : "As patient and generous as life, as harsh and merciless as nature.",
  "illustration_id"   : "82c0ea68-cf37-41ec-aec7-123e8e9f05d3",
  "artist"            : "Chris Rahn",
  "frame"             : "2015",
  "full_art"          : false,
  "border_color"      : "black",
  "timeshifted"       : false,
  "colorshifted"      : false,
  "futureshifted"     : false,
  "edhrec_rank"       : 128,
  "eur"               : "0.10",
  "related_uris": {
    "tcgplayer_decks" : "http://decks.tcgplayer.com/magic/deck/search?contains=Llanowar+Elves&page=1&partner=Scryfall",
    "edhrec"          : "http://edhrec.com/route/?cc=Llanowar+Elves",
    "mtgtop8"         : "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Llanowar+Elves"
  },
  "purchase_uris"     : {
    "amazon"          : "https://www.amazon.com/gp/search?ie=UTF8&index=toys-and-games&keywords=Llanowar+Elves&tag=scryfall-20",
    "ebay"            : "http://rover.ebay.com/rover/1/711-53200-19255-0/1?campid=5337966903&icep_catId=19107&icep_ff3=10&icep_sortBy=12&icep_uq=Llanowar+Elves&icep_vectorid=229466&ipn=psmain&kw=lg&kwid=902099&mtid=824&pub=5575230669&toolid=10001",
    "tcgplayer"       : "https://scryfall.com/s/tcgplayer/168646",
    "magiccardmarket" : "https://scryfall.com/s/mcm/359654",
    "cardhoarder"     : "https://www.cardhoarder.com/cards?affiliate_id=scryfall&data%5Bsearch%5D=Llanowar+Elves&ref=card-profile&utm_campaign=affiliate&utm_medium=card&utm_source=scryfall",
    "card_kingdom"    : "https://www.cardkingdom.com/catalog/search?filter%5Bname%5D=Llanowar+Elves&partner=scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall",
    "mtgo_traders"    : "http://www.mtgotraders.com/store/search.php?q=Llanowar+Elves&referral=scryfall",
    "coolstuffinc"    : "https://www.coolstuffinc.com/main_search.php?pa=searchOnName&page=1&q=Llanowar+Elves&resultsPerPage=50&utm_source=scryfall"
  }
}

--------------------------------------------------
--------------------------------------------------

{-|

-}

--------------------------------------------------
{--------------------------------------------------






--------------------------------------------------
--------------------------------------------------

{-|

e.g.

@
"" :: ''
@

-}

newtype  = 

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsString Identifier where
  fromString = coerce





-------------------------------------------------}