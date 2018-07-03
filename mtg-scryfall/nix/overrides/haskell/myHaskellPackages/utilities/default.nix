{ lib
, fetchgit
, haskellPackages
}:

########################################

# (see the "NOTES" section at the end of this file for documention)

########################################
# "private"
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
# "public"
rec {

callPrefetchedHaskellPackageByJSONFile = p:
 haskellPackages.callCabal2nix
  (builtins.baseNameOf p) 
  (fetchPrefetched p);

callPrefetchedNixPackageByJSONFile = p:
 haskellPackages.callPackage
  (fetchPrefetched p);

# callHackageByReviseableVersion = s: #TODO revision
#  let
#  {version,revision} = parseReviseableVersion s;
#  in
#  if   revision == 0
#  then haskellPackages.callHackage      name version
#  else haskellPackages.callHackageWith? name version revision;

}
########################################
/* NOTES

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