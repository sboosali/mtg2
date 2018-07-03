#!/bin/bash
set -e
########################################

########################################

PAGE_RANGE=$(seq 1 20)

URL="https://img.scryfall.com/cards"

DIRECTORY=./data/scryfall/images

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
 fi
 
 sleep "$DELAY"
 
done

########################################
# "After"

find "$DIRECTORY"

find "$DIRECTORY" | wc -l

########################################

# USAGE
# $ ./scripts/get-images-from-scryfall.sh

# e.g. 
# $ curl "https://img.scryfall.com/cards/png/en/c16/143.png" > ./data/large/scryfall/images/png/en-c16-143.png
# $ feh ./data/large/scryfall/images/png/en-c16-143.png

# e.g.
# { "object": "card"
# , "name":"Duskdale Wurm"
# , "printed_name":"Duskdale Wurm"
# , "lang":"en"
# ...
# , "highres_image": true,
# , "image_uris":
#     { "small":       "https://img.scryfall.com/cards/small/en/m13/166.jpg?1527473415"
#     , "normal":      "https://img.scryfall.com/cards/normal/en/m13/166.jpg?1527473415"
#     , "large":       "https://img.scryfall.com/cards/large/en/m13/166.jpg?1527473415"
#     , "png":         "https://img.scryfall.com/cards/png/en/m13/166.png?1527473415"
#     , "art_crop"     "https://img.scryfall.com/cards/art_crop/en/m13/166.jpg?1527473415"
#     , "border_crop": "https://img.scryfall.com/cards/border_crop/en/m13/166.jpg?1527473415"
#     }
# }
# 
# { "object": "card"
# , "name":"Duskdale Wurm"
# , "printed_name":"暮谷亚龙"
# , "lang":"zhs"
# ...
# , "highres_image": false,
# , "image_uris":
#     { "small":       "https://img.scryfall.com/cards/small/zhs/m13/166.jpg?1527473415"
#     , "normal":      "https://img.scryfall.com/cards/normal/zhs/m13/166.jpg?1527473415"
#     , "large":       "https://img.scryfall.com/cards/large/zhs/m13/166.jpg?1527473415"
#     , "png":         "https://img.scryfall.com/cards/png/zhs/m13/166.png?1527473415"
#     , "art_crop"     "https://img.scryfall.com/cards/art_crop/zhs/m13/166.jpg?1527473415"
#     , "border_crop": "https://img.scryfall.com/cards/border_crop/zhs/m13/166.jpg?1527473415"
#     }
# }

#TODO $ curl "https://img.scryfall.com/cards?image=png" > data/scryfall/images/0003.json

# NOTE image-formats:
#
# - "?image=png"
#    745×1040 resolution
#    PNG format
#    '''A transparent, rounded full card PNG. This is the best image to use for videos or other high-quality content.'''
#
# - "?image=art_crop"
#    (resolution varies)
#    JPG format
#    '''A rectangular crop of the card’s art only. Not guaranteed to be perfect for cards with outlier designs or strange frame arrangements'''
#
# - 
# - 

