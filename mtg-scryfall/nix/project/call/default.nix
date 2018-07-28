########################################
selfSystem:  superSystem:                # e.g. `nixpkgs`

selfHaskell: superHaskell:               # e.g. `nixpkgs.haskellPackages`

########################################
let

# "PRIVATE" functions

fetchPrefetched = p:
 (superSystem.fetchgit (importPrefetched p));

importPrefetched = p:
 matchPrefetched (superSystem.lib.importJSON p);

matchPrefetched = { url, rev, sha256, fetchSubmodules ? false, ... }:
 { inherit url rev sha256 fetchSubmodules; };

in
########################################
let

# "PRIVATE" functions

filterCabalNewBuildSource =
  builtins.filterSource isCabalNewBuildSourceFile;

isCabalNewBuildSourceFile = filepath: filetype:
  let

  filename = builtins.baseNameOf filepath;

  regexDontMatch = regex: string:
   null == builtins.match regex string;

  in

  (if   "regular" == filetype
   then regexDontMatch "\.ghc.environment\..*" filename
   then regexDontMatch ".*\.project\.local"    filename
   else true)

  # && builtins.baseNameOf filepath != "..."

  ;

# ^ e.g. 
# 
#   mkDerivation {
#     src = filterCabalNewBuildSource ../../spiros;
#   }
#

in
########################################
let

callPackage = path:
 callPackageWith path {};

callPackageWith =
 selfHaskell.callPackage;

in
########################################
let

callHackage = arguments:
 callHackageWith arguments {};

callHackageWith = { pname, version, revision ? 0 }:
 selfHaskell.callHackage pname version;
 #TODO revision

in
########################################
let

callCabal = arguments:
 callCabalWith arguments {};

callCabalWith = { path, pname ? "" }:       # ? builtins.baseNameOf path }:
 let
 filteredPath = path;                       # filterCabalNewBuildSource path;
 in
 selfHaskell.callCabal2nix pname filteredPath;

# ^ NOTE in both those commented-out lines above, 
# the functions-on-paths are dropped/simplified,
# because of this error message from nix-2.0:
# "the string ... is not allowed to refer to a store path".

in
########################################
let

callPrefetched = arguments:
 callPrefetchedWith arguments {};

callPrefetchedWith = jsonPath:           # e.g. jsonPath = ./<...>/reflex.json  # (filepath)
                                         # e.g. jsonPath = "<...>/reflex.json"  # (string)
 let     
 path = fetchPrefetched jsonPath;        # e.g. path = <fetchgit   ...> 
 in                                      #           = /nix/store/...-reflex-.../

 callCabalWith { inherit path; };         # e.g. pname = <baseNameOf ...> 
                                          #           = "reflex"
in
########################################
let

# callCabalProject = arguments:
#  callCabalProjectWith arguments {};

# callCabalProjectWith = {}:
#  ...

in
########################################
let


in
########################################
{

 inherit callPackage    callPackageWith;

 inherit callHackage    callHackageWith;

 inherit callCabal      callCabalWith;

 # ^ tweaked, but official, from `haskell.packages`
 ####################

 inherit callPrefetched   callPrefetchedWith;

 #TODO inherit callCabalProject callCabalProjectWith;

 # ^ custom, from my `haskell.project`
 ####################

}
########################################
/*

====================

TODO subdirectory for *everything*

(callCabalProject { path = ../reflex-dom }).reflex-dom
(callCabalProject { path = ../reflex-dom }).reflex-dom-core

callCabal { path = ../reflex-dom; subdirectory = "reflex-dom";      pname = "reflex-dom"; }
callCabal { path = ../reflex-dom; subdirectory = "reflex-dom-core"; pname = "reflex-dom-core"; }

====================

  > callCabal { path = ../spiros; }

is the same as:

  > callCabal { path = ../spiros; pname = "spiros"; }

which is:

  > callCabal2nix "spiros" ../spiros

====================

callPrefetchedWith = path:           # e.g. jsonPath = ./<...>/reflex.json  # (filepath)
                                         # e.g. jsonPath = "<...>/reflex.json"  # (string)
 let
 name = (builtins.baseNameOf path);      # e.g. <baseNameOf ...> = "reflex"
 path = (fetchPrefetched     path);      # e.g. <fetchgit   ...> = /nix/store/...-reflex-.../
 in

 selfHaskell.callCabal2nix name path;

====================

callCabal2nix

  $
  ...
  while evaluating the derivation attribute 'name' at /home/sboo/nixpkgs/pkgs/stdenv/generic/make-derivation.nix:170:11:
  the string 'cabal2nix-s43kka3k771y7b1qwrb1vbg8ygg0facf-reflex-9fcbf07' is not allowed to refer to a store path (such as '!out!/nix/store/ymxlx320vv2bpqds5lz13ky7y8ji1nvd-reflex-9fcbf07.drv'), at /nix/store/0d60i73mcv8z1m8d2m74yfn84980gfsa-nix-2.0.4/share/nix/corepkgs/derivation.nix:8:12

i.e. error:

  "the string" "is not allowed to refer to a store path"

issue:

  https://github.com/NixOS/nix/issues/1166



========================================

*/
########################################