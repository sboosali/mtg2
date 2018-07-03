{ lib
, fetchgit
# , utilities
}:

#TODO
# { localIdentifiers
# , gitIdentifiers
# , hackageIdentifiers
# }: 

haskellPackages:

# (see the "NOTES" section at the end of this file for documention)


########################################
# "utilities"
let

fetchPrefetched = p:
 (fetchgit (importPrefetched p));

importPrefetched = p:
 matchPrefetched (lib.importJSON p);

matchPrefetched = { url, rev, sha256, fetchSubmodules ? false, ... }:
 { inherit url rev sha256 fetchSubmodules; };

# :: String -> ReviseableVersion
parseReviseableVersion = s:
 { version  = s; 
   revision = 0;
 };

 # ^ e.g. { version = "2.3.0.0"; revision = 1; }
 # TODO splitOn "-"

in
########################################

# callHackageByReviseableVersion = s: #TODO revision
#  let
#  {version,revision} = parseReviseableVersion s;
#  in
#  if   revision == 0
#  then haskellPackages.callHackage      name version
#  else haskellPackages.callHackageWith? name version revision;

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
 haskellPackages.callCabal2nix name (fetchPrefetched path) {};

 # utilities.callPrefetchedHaskellPackageByJSONFile path {}; #TODO: use `name`.
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

  > builtins.baseNameOf "https://github.com/bennofs/reflex-host"
  "reflex-host"

====================

  $ nix-prefetch-git > https://github.com/.../PACKAGE > PACKAGE.json
  > (fetchPrefetched ./PACKAGE.json)

====================

`matchPrefetched` is almost identity on the record that `nix-prefetch-git` outputs.

safer:
- default missing optional parameters.
- ignore present irrelevant parameters.

====================

*/
########################################