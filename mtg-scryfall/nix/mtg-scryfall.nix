{ haskellPackages
, haskellLibrary
}:

dependencies:

########################################

#haskellPackages.reflex-sdl2

haskellPackages.callCabal2nix "mtg-scryfall" ./.. dependencies

########################################