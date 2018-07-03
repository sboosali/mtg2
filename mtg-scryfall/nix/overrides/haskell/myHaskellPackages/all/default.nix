parameters@
{ lib
, utilities
}:

#TODO
# { localIdentifiers
# , gitIdentifiers
# , hackageIdentifiers
# }: 

haskellPackages:

# (see the "NOTES" section at the end of this file for documention)

########################################
let

localIdentifiers = (import ../local);

# ^ e.g. { spiros = ~/haskell/spiros; }
# ^ TODO e.g. { spiros = haskellPackages.mkDerivation { ... }; }

gitIdentifiers = (import ../git);

# ^ e.g. { reflex = ./reflex.json; }
# ^ TODO e.g. { reflex = { url = ...; rev = ...; sha256 = ...; }

hackageIdentifiers = (import ../hackage);

# ^ e.g. { Cabal = "2.3.0.0"; }
# ^ TODO e.g. { Cabal = { version = "2.3.0.0"; revision = 1; }; }
# ^ TODO e.g. { Cabal = "2.3.0.0-1"; }

in
########################################
let

fromDirectoryPath = name: path:
 haskellPackages.callCabal2nix name path {};

 # ^ e.g. { spiros = <cabal2nix> "spiros" ~/haskell/spiros; }

localPackages = lib.mapAttrs
 fromDirectoryPath
 localIdentifiers;

in
########################################
let

fromPrefetchedJsonPath = name: path:
 utilities.callPrefetchedHaskellPackageByJSONFile path {};

 # ^ e.g. { reflex = ...; }

gitPackages = lib.mapAttrs
 fromPrefetchedJsonPath 
 gitIdentifiers;

in
########################################
let

fromVersion = name: version:
 haskellPackages.callHackage name version {};

hackagePackages = lib.mapAttrs
 fromVersion
 hackageIdentifiers;

in
########################################
let

allPackages =
 hackagePackages //
 gitPackages     //
 localPackages   ;

in
########################################

allPackages

########################################
/* NOTES

====================

the customized `local` packages override "unpublished" `git`(hub) packages,
which override `hackage` packages.

====================

> lib.mapAttrs (k: v: <f> k v) { "k1" = v1; "k2" = v2; ... }

> lib.mapAttrs  (k: v:                      lib.stringLength v)  { "k1" = "v1"; "k2" = "v2"; }
> lib.mapAttrs' (k: v: lib.nameValuePair v (lib.stringLength v)) { v1   = 2;      v2 = 2;    }

====================

`callCabal2nix` searches `path` for "`name`.cabal". (i.e. `name` isn't cosmetic).

====================

*/
########################################