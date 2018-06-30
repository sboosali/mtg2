#!/bin/bash
set -e
########################################

########################################

PAGE_RANGE=$(seq 1 1075)

URL="https://api.scryfall.com/cards"

DIRECTORY=./data/large/scryfall/cards

DELAY=0.5
# ^ in seconds.

########################################
# "Before"

mkdir -p "$DIRECTORY"

########################################
# "During"

for PAGE in $PAGE_RANGE; do
 
 # ^ e.g. "PAGE=3"
 
 NAME=$(printf "%04d" "$PAGE")
 # ^ pad to four-digits.
 # e.g. "0003"
 
 FILE="$DIRECTORY/$NAME.json"
 # ^ e.g. ./data/scryfall/cards/0003.json
 
 QUERY="$URL?page=$PAGE"
 # ^ e.g. https://api.scryfall.com/cards?page=3
 
 if [ -f "$FILE" ]; then
    echo "[Skipping    $FILE] it already exists."
 else
    echo "[Downloading $FILE]"
    curl "$QUERY" > "$FILE" || true
    # ^ continue, ignoring errors.
    sleep "$DELAY"
    # ^ rate-limit ourselves (we're crawling a free API).
 fi
 
done

########################################
# "After"

find "$DIRECTORY"

find "$DIRECTORY" | wc -l

########################################

# USAGE
# $ ./scripts/get-cards-from-scryfall.sh
# $ find ./data/scryfall/cards

# e.g. 
# curl "https://api.scryfall.com/cards?page=3" > data/scryfall/cards/0003.json

# NOTE
# - Scryfall currently [circa 2018] has 188,091 cards;
# - this endpoint [/cards] has 1075 pages;
# - representing over 400 MB of JSON data
