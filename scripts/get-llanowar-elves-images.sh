#!/bin/bash

##################################################

Directory="./data/examples"

function sboo-scryfall-download () {
    File="${1}"
    Link="${2}"

    wget "$Link" --output-document "$Directory/$File"
}

##################################################

mkdir -p "${Directory}"

sboo-scryfall-download "LlanowarElves-small.jpg"      "https://img.scryfall.com/cards/small/en/m19/314.jpg?1528742053"
sboo-scryfall-download "LlanowarElves-normal.jpg"     "https://img.scryfall.com/cards/normal/en/m19/314.jpg?1528742053"
sboo-scryfall-download "LlanowarElves-large.jpg"      "https://img.scryfall.com/cards/large/en/m19/314.jpg?1528742053"
sboo-scryfall-download "LlanowarElves-png.png"        "https://img.scryfall.com/cards/png/en/m19/314.png?1528742053"
sboo-scryfall-download "LlanowarElves-artcrop.jpg"    "https://img.scryfall.com/cards/art_crop/en/m19/314.jpg?1528742053"
sboo-scryfall-download "LlanowarElves-bordercrop.jpg" "https://img.scryfall.com/cards/border_crop/en/m19/314.jpg?1528742053"

##################################################
