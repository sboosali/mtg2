#!/bin/bash
set -e
########################################






# https://archive.scryfall.com/json/scryfall-all-cards.json

# CONSTANTS

Color_Off='\033[0m'           # Reset any previous formatting.

Red='\033[0;31m'              # Red.

Red_Bold='\033[1;31m'         # Red, bold.

Black_Underlined='\033[4;30m' # Black, underlined.

########################################
# CHECKS

if   command -v curl
then
  echo "[CHECK passed] the {{{ curl }}} command is present."
else
  >&2 echo -e '\n--------------------------------------------------------------------------------\n[CHECK failed] the {{{ curl }}} command is absent.\n               this script requires {{{ curl }}}, which downloads files by url.\n               you should provision the environment with this command (between the braces): {{{ nix-shell ./scripts/shell.nix }}}. or you can install it directly with your package manager; for example, for the {{{ nix }}} package manager, with this command: {{{ nix-env -i curl }}}.\n--------------------------------------------------------------------------------'
  exit 1;
fi

# ^ fail early if the required `command`s aren't present.
#
# the `set -e` (above) aborts the whole file on any error;
# an "error" means "an expression/command returned non-zero".

########################################
# SCRIPT: VARIABLES

PAGE_RANGE=$(seq 1 1075)

URL="https://api.scryfall.com/cards"

DIRECTORY=./data/large/scryfall/cards

DELAY=0.5
# ^ in seconds.

_FAILED=
# ^ :: pseudo-Bool
#   a blank environment-variable assignment (e.g. `<X>=`) means "it's set but empty".

########################################
# SCRIPT: BEFOREHAND

mkdir -p "$DIRECTORY"

########################################
# SCRIPT: ACTIONS

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
    if curl "$QUERY" > "$FILE"; then
       # ^ (whether the command succeeded)
       sleep "$DELAY"
       # ^ rate-limit ourselves (we're crawling a free API).
    else
       # ^ continue, ignoring errors. but, also warn the user.
       _FAILED=1;
    fi
 fi

done

########################################
# SCRIPT: AFTERWARDS

if [ -z "$_FAILED" ]; then
   # ^ (whether either [1] the environment-variable is unset, or [2] the string is empty)
   find "$DIRECTORY" | sort
else
   # ^ (then both [1] the variable is non-null and [2] the string is non-empty)
   >&2 echo "[WARNING] some downloads failed."
fi

find "$DIRECTORY" | wc -l

########################################
# NOTES

# USAGE
# $ ./scripts/get-cards-from-scryfall.sh
# $ find ./data/scryfall/cards

# e.g. 
# curl "https://api.scryfall.com/cards?page=3" > data/scryfall/cards/0003.json

# NOTE
# - Scryfall currently [circa 2018] has 188,091 cards;
# - this endpoint [/cards] has 1075 pages;
# - representing over 400 MB of JSON data

########################################