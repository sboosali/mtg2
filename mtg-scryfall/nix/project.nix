packageParameters:

########################################
{

 mtg-scryfall = (import ./mtg-scryfall.nix) packageParameters {
 };

}
########################################
/* NOTES

====================
TODO

fromFileWith = path: arguments: dependencies:
 (import path) args dependencies;

fromFile     = path: arguments: 
 fromFileWith path arguments {};

mtg-scryfall = fromFileWith ./mtg-scryfall.nix packageParameters {
};

====================

mtg-scryfall = ??? ./mtg-scryfall.nix;

====================

*/
########################################