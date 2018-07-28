with import <nixpkgs> {};

haskellPackages.extend
 (haskell.lib.packageSourceOverrides {

  mtg-scryfall = ./mtg-scryfall;

 })

