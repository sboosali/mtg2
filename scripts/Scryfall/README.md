# `scryfall` scripts

## Usage

each `.sh` script wraps an `.hs` file, like so:

```sh
cabal new-run ${ScriptName}.hs -- ${ScriptOptions}
```

## Files

1. `./GetCards.hs`: 

1. `./DownloadImages.hs`: 

1. `./Main.hs`: 

## Notes

the empty `cabal.project` increases the reproducibility of the scripts, by shadows `mtg`'s (project-specific) and my (user-wide) `.project` files. 

## 