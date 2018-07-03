projectConfiguration:

self:
super:

# ^ self/super are global `pkgs`;
# an overlay for `nixpkgs { overlays = [...]; }`.

########################################
let

dependencies = 
  { inherit (self)  fetchgit;
    inherit (super) lib;
  };

in
########################################
let

originalHaskellPackages = 
 (import ../haskellPackages) projectConfiguration self;

overridenHaskellPackages =
 (import ../myHaskellPackages) dependencies originalHaskellPackages; #TODO separate directory

in
########################################

overridenHaskellPackages

########################################
/* NOTES

- an overlay for `nixpkgs { overlays = [...]; }`.

the two `<self|super>HaskellPackages` are:
- an override for `haskellPackages.override { overrides = ...; }`.

====================

each `self` and `super` is scoped at: 
- `nixpkgs.pkgs`,            for Global; 
- `nixpkgs.haskellPackages`, for Haskell.

====================

*/
########################################