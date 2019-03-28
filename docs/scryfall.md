# Scryfall

<https://scryfall.com/docs/api/cards>

## Scryfall API

the Card object has several fields, grouped into these groups:

* Core Field
* Gameplay Field
* Print Field




All Cards	Download	63.5 MB	2019-02-18 10:56 UTC
A JSON file containing every card object on Scryfall in every language.


https://archive.scryfall.com/json/scryfall-all-cards.json



Rulings	Download	2.09 MB	2019-02-18 10:11 UTC
A JSON file containing all Rulings on Scryfall. Each ruling refers to cards via an `oracle_id`.







Multiface Cards
Magic cards can have multiple faces. The faces could be shown divided on the front of the card as in split cards and flip cards, or the card can be double-sided as in transform cards and double-sided tokens.




Layouts and Frames
Scryfall tracks the appearance of cards programmatically using several properties: layout, frame, and frame_effect.




Layout
The layout property categorizes the arrangement of card parts, faces, and other bounded regions on cards. The layout can be used to programmatically determine which other properties on a card you can expect.

Specifically:

Cards with the layouts split, flip, transform, and double_faced_token will always have a card_faces property describing the distinct faces.
Cards with the layout meld will always have a related_cards property pointing to the other meld parts.




LAYOUT	DESCRIPTION	EXAMPLES
normal

A standard Magic card with one face

Example Cards

split

A split-faced card

Example Cards

flip

Cards that invert vertically with the flip keyword

Example Cards

transform

Double-sided cards that transform

Example Cards

meld

Cards with meld parts printed on the back

Example Cards

leveler

Cards with Level Up

Example Cards

saga

Saga-type cards

Example Cards

planar

Plane and Phenomenon-type cards

Example Cards

scheme

Scheme-type cards

Example Cards

vanguard

Vanguard-type cards

Example Cards

token

Token cards

Example Cards

double_faced_token

Tokens with another token printed on the back

Example Cards

emblem

Emblem cards

Example Cards

augment

Cards with Augment

Example Cards

host

Host-type cards

Example Cards







Frames
The frame field tracks the major edition of the card frame of used for the re/print in question. The frame has gone though several major revisions in Magic’s lifetime.

FRAME	DESCRIPTION	EXAMPLES
1993

The original Magic card frame, starting from Limited Edition Alpha.

Example Cards

1997

The updated classic frame starting from Mirage block

Example Cards

2003

The “modern” Magic card frame, introduced in Eighth Edition and Mirrodin block.

Example Cards

2015

The holofoil-stamp Magic card frame, introduced in Magic 2015.

Example Cards

future

The frame used on cards from the future

Example Cards









Languages
Scryfall archives Magic cards in 17 languages (including some unofficial languages).

Each Card object indicates its language in the lang property using an ISO-like code. When available, cards may also include their printed text in printed_name, printed_type_line, and printed_text fields.

The current languages are:

CODE	PRINTED CODE	LANGUAGE	CARDS	EXAMPLES
en

en

English

44,106

Example Cards

es

sp

Spanish

25,582

Example Cards

fr

fr

French

26,084

Example Cards

de

de

German

25,486

Example Cards

it

it

Italian

26,365

Example Cards

pt

pt

Portuguese

22,545

Example Cards

ja

jp

Japanese

20,411

Example Cards

ko

kr

Korean

7,527

Example Cards

ru

ru

Russian

13,036

Example Cards

zhs

cs

Simplified Chinese

16,908









id	UUID		A unique ID for this card in Scryfall’s database.


lang	String		A language code for this printing.


multiverse_ids	Array	
Nullable
This card’s multiverse IDs on Gatherer, if any, as an array of integers. Note that Scryfall includes many promo cards, tokens, and other esoteric objects that do not have these identifiers.

object	String		A content type for this object, always card.


oracle_id	UUID		A unique ID for this card’s oracle identity. This value is consistent across reprinted card editions, and unique among different cards with the same name (tokens, Unstable variants, etc.

rulings_uri	URI		A link to this card’s rulings list on Scryfall’s API.


scryfall_uri	URI		A link to this card’s permapage on Scryfall’s website.

all_parts	Array	
Nullable
If this card is closely related to other cards, this property will be an array with Related Card Objects.


card_faces	Array	
Nullable
An array of Card Face objects, if this card is multifaced.

layout	String		A code for this card’s layout.


legalities	Object		An object describing the legality of this card across play formats. Possible legalities are legal, not_legal, restricted, and banned.

name	String		The name of this card. If this card has multiple faces, this field will contain both names separated by ␣//␣.





border_color	String		This card’s border color: black, borderless, gold, silver, or white.

frame	String		This card’s frame layout.


full_art	Boolean		True if this card’s artwork is larger than normal.


printed_name	String	
Nullable
The localized name printed on this card, if any.


printed_text	String	
Nullable
The localized text printed on this card, if any.


printed_type_line	String	
Nullable
The localized type line printed on this card, if any.









id	UUID		An unique ID for this card in Scryfall’s database.
object	String		A content type for this object, always related_card.
component	String		A field explaining what role this card plays in this relationship, one of token, meld_part, meld_result, or combo_piece.
name	String		The name of this particular related card.





## Scryfall Types

e.g. `Llanowar Elves` (full):

```json
{
  "object": "card",
  "id": "73542493-cd0b-4bb7-a5b8-8f889c76e4d6",
  "oracle_id": "68954295-54e3-4303-a6bc-fc4547a4e3a3",
  "multiverse_ids": [],
  "name": "Llanowar Elves",
  "lang": "en",
  "uri": "https://api.scryfall.com/cards/m19/314",
  "scryfall_uri": "https://scryfall.com/card/m19/314?utm_source=api",
  "layout": "normal",
  "highres_image": false,
  "image_uris": {
    "small": "https://img.scryfall.com/cards/small/en/m19/314.jpg?1528742053",
    "normal": "https://img.scryfall.com/cards/normal/en/m19/314.jpg?1528742053",
    "large": "https://img.scryfall.com/cards/large/en/m19/314.jpg?1528742053",
    "png": "https://img.scryfall.com/cards/png/en/m19/314.png?1528742053",
    "art_crop": "https://img.scryfall.com/cards/art_crop/en/m19/314.jpg?1528742053",
    "border_crop": "https://img.scryfall.com/cards/border_crop/en/m19/314.jpg?1528742053"
  },
  "mana_cost": "{G}",
  "cmc": 1,
  "type_line": "Creature — Elf Druid",
  "oracle_text": "{T}: Add {G}.",
  "power": "1",
  "toughness": "1",
  "colors": [
    "G"
  ],
  "color_identity": [
    "G"
  ],
  "legalities": {
    "standard": "legal",
    "future": "legal",
    "frontier": "legal",
    "modern": "legal",
    "legacy": "legal",
    "pauper": "legal",
    "vintage": "legal",
    "penny": "not_legal",
    "commander": "legal",
    "1v1": "legal",
    "duel": "legal",
    "brawl": "legal"
  },
  "reserved": false,
  "foil": false,
  "nonfoil": true,
  "oversized": false,
  "reprint": true,
  "set": "m19",
  "set_name": "Core Set 2019",
  "set_uri": "https://api.scryfall.com/sets/m19",
  "set_search_uri": "https://api.scryfall.com/cards/search?order=set&q=e%3Am19&unique=prints",
  "scryfall_set_uri": "https://scryfall.com/sets/m19?utm_source=api",
  "rulings_uri": "https://api.scryfall.com/cards/m19/314/rulings",
  "prints_search_uri": "https://api.scryfall.com/cards/search?order=set&q=%21%E2%80%9CLlanowar+Elves%E2%80%9D&unique=prints",
  "collector_number": "314",
  "digital": false,
  "rarity": "common",
  "flavor_text": "As patient and generous as life, as harsh and merciless as nature.",
  "illustration_id": "82c0ea68-cf37-41ec-aec7-123e8e9f05d3",
  "artist": "Chris Rahn",
  "frame": "2015",
  "full_art": false,
  "border_color": "black",
  "timeshifted": false,
  "colorshifted": false,
  "futureshifted": false,
  "edhrec_rank": 128,
  "eur": "0.10",
  "related_uris": {
    "tcgplayer_decks": "http://decks.tcgplayer.com/magic/deck/search?contains=Llanowar+Elves&page=1&partner=Scryfall",
    "edhrec": "http://edhrec.com/route/?cc=Llanowar+Elves",
    "mtgtop8": "http://mtgtop8.com/search?MD_check=1&SB_check=1&cards=Llanowar+Elves"
  },
  "purchase_uris": {
    "amazon": "https://www.amazon.com/gp/search?ie=UTF8&index=toys-and-games&keywords=Llanowar+Elves&tag=scryfall-20",
    "ebay": "http://rover.ebay.com/rover/1/711-53200-19255-0/1?campid=5337966903&icep_catId=19107&icep_ff3=10&icep_sortBy=12&icep_uq=Llanowar+Elves&icep_vectorid=229466&ipn=psmain&kw=lg&kwid=902099&mtid=824&pub=5575230669&toolid=10001",
    "tcgplayer": "https://scryfall.com/s/tcgplayer/168646",
    "magiccardmarket": "https://scryfall.com/s/mcm/359654",
    "cardhoarder": "https://www.cardhoarder.com/cards?affiliate_id=scryfall&data%5Bsearch%5D=Llanowar+Elves&ref=card-profile&utm_campaign=affiliate&utm_medium=card&utm_source=scryfall",
    "card_kingdom": "https://www.cardkingdom.com/catalog/search?filter%5Bname%5D=Llanowar+Elves&partner=scryfall&utm_campaign=affiliate&utm_medium=scryfall&utm_source=scryfall",
    "mtgo_traders": "http://www.mtgotraders.com/store/search.php?q=Llanowar+Elves&referral=scryfall",
    "coolstuffinc": "https://www.coolstuffinc.com/main_search.php?pa=searchOnName&page=1&q=Llanowar+Elves&resultsPerPage=50&utm_source=scryfall"
  }
}
```

e.g. `Llanowar Elves` (terse):


```json
{

  "name": "Llanowar Elves",
  "lang": "en",

  "object": "card",
  "id": "73542493-cd0b-4bb7-a5b8-8f889c76e4d6",
  "oracle_id": "68954295-54e3-4303-a6bc-fc4547a4e3a3",
  "multiverse_ids": [],
  "uri": "https://api.scryfall.com/cards/m19/314",

  "highres_image": false,
  "image_uris": {
    "small": "https://img.scryfall.com/cards/small/en/m19/314.jpg?1528742053",
    "normal": "https://img.scryfall.com/cards/normal/en/m19/314.jpg?1528742053",
    "large": "https://img.scryfall.com/cards/large/en/m19/314.jpg?1528742053",
    "png": "https://img.scryfall.com/cards/png/en/m19/314.png?1528742053",
    "art_crop": "https://img.scryfall.com/cards/art_crop/en/m19/314.jpg?1528742053",
    "border_crop": "https://img.scryfall.com/cards/border_crop/en/m19/314.jpg?1528742053"
  },

  "mana_cost": "{G}",
  "cmc": 1,
  "type_line": "Creature — Elf Druid",
  "oracle_text": "{T}: Add {G}.",
  "power": "1",
  "toughness": "1",
  "colors": [
    "G"
  ],
  "color_identity": [
    "G"
  ],
  "layout": "normal",

  "reprint": true,
  "rulings_uri": "https://api.scryfall.com/cards/m19/314/rulings",
}
```

e.g. `Llanowar Elves` (images):

* "./scripts/data/examples/LlanowarElves-small.jpg"      "https://img.scryfall.com/cards/small/en/m19/314.jpg?1528742053"
* "./scripts/data/examples/LlanowarElves-normal.jpg"     "https://img.scryfall.com/cards/normal/en/m19/314.jpg?1528742053"
* "./scripts/data/examples/LlanowarElves-large.jpg"      "https://img.scryfall.com/cards/large/en/m19/314.jpg?1528742053"
* "./scripts/data/examples/LlanowarElves-png.png"        "https://img.scryfall.com/cards/png/en/m19/314.png?1528742053"
* "./scripts/data/examples/LlanowarElves-artcrop.jpg"    "https://img.scryfall.com/cards/art_crop/en/m19/314.jpg?1528742053"
* "./scripts/data/examples/LlanowarElves-bordercrop.jpg" "https://img.scryfall.com/cards/border_crop/en/m19/314.jpg?1528742053"

## Scryfall Images

the `png` images are the highest quality. 

the `artcrop` images are just the artwork (no card text, border, etc).

## Scryfall Data

GET <https://api.scryfall.com/catalog/card-names>

```sh
cd ./mtg-scryfall/data/json/
wget https://api.scryfall.com/catalog/card-names
```


## Scryfall 

