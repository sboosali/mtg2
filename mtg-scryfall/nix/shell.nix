{ nixpkgs       
   ? import <nixpkgs> { overlays = []; }
#   ? import ./nixpkgs {}

, compilerName   ? null  # :: Maybe String 
, compilerFlavor ? null  # :: Maybe String

, withProfiling  ? true  # :: Bool
, withHoogle     ? true  # :: Bool

, development    ? true
}:

/*

*/

########################################
let

inherit (nixpkgs) pkgs;

#TODO
self = pkgs;
super = pkgs;

#TODO
inherit (pkgs) lib;
inherit (pkgs) fetchgit;

in
########################################
let

isCabalNewBuildSourceFile = filepath: filetype:
  true

  && (if   "regular" == filetype
      then null      == builtins.match
                        "\.ghc.environment\..*"
                         (builtins.baseNameOf filepath)
      else true)

  # && builtins.baseNameOf filepath != "..."

  ;

filterCabalNewBuildSource = 
  builtins.filterSource isCabalNewBuildSourceFile;

# ^ e.g. 
# 
#   mkDerivation {
#     src = filterCabalNewBuildSource ../../spiros;
#   }
#

in
########################################
let

projectConfiguration = 
 { haskellCompiler =
    { name   = compilerName;
      flavor = compilerFlavor;
    };
  haskellFlags = 
    { profiling = withProfiling;
      hoogle    = withHoogle; 
    };
 };

in
########################################
let

fetchPrefetched = p:
 (fetchgit (importPrefetched p));

importPrefetched = p:
 matchPrefetched (lib.importJSON p);

matchPrefetched = { url, rev, sha256, fetchSubmodules ? false, ... }:
 { inherit url rev sha256 fetchSubmodules; };

in
########################################
let

# haskell =
#  (import ./overrides/haskell) projectConfiguration self super;

haskell = {
 packages = super.haskellPackages;
 compiler = null;
};

haskellPackagesPlusUtilities = haskell.packages.override {
  overrides = self: super: {

    # callPrefetchedJSON = path:     # e.g. path = ./<...>/reflex.json (or, "<...>/reflex.json").
    #   self.callCabal2nix
    #     (builtins.baseNameOf path) # e.g. <baseNameOf ...> = "reflex"
    #     (fetchPrefetched     path) # e.g. <fetchgit   ...> = ...
    #     ;

    #TODO
    # callPrefetchedJSON = path:     # e.g. path = "~/haskell/spiros"
    #   self.callCabal2nix
    #     (builtins.baseNameOf path) # e.g. <baseNameOf ...> = "spiros"
    #     (fetchPrefetched     path) # e.g. <fetchgit   ...> = "~/haskell/spiros"
    #     ;

  };
};

haskellPackagesPlusPackages = haskellPackagesPlusUtilities.override {
  overrides = self: super:

    let

    callPrefetchedJSON = path:     # e.g. path = ./<...>/reflex.json (or, "<...>/reflex.json").
      self.callCabal2nix
        (builtins.baseNameOf path) # e.g. <baseNameOf ...> = "reflex"
        (fetchPrefetched     path) # e.g. <fetchgit   ...> = ...
        ;

    in

 {

    spiros =
      self.callCabal2nix "spiros" (filterCabalNewBuildSource ~/haskell/spiros) {};
    
    # reflex = callPrefetchedJSON ./overrides/haskell/myHaskellPackages/git/reflex.json; #TODO self.callPrefetchedJSON #TODO shorten path

    # spiros = self.callCabal2nix "spiros" 
    #           (fetchPrefetched ./overrides/haskell/myHaskellPackages/git/spiros.json) {}; 

    reflex = self.callCabal2nix "reflex"
              (fetchPrefetched ./overrides/haskell/myHaskellPackages/git/reflex.json) {}; 
  };
};

haskellPackagesPlusEverything = haskellPackagesPlusPackages;

####################

packageParameters = {
  haskellPackages = haskellPackagesPlusPackages;
  haskellLibrary  = pkgs.haskell.lib;
};

thisProject =
 (import ./project.nix) packageParameters;

####################

# TODO merge multiple dependencies into one shell.
thisDerivation  = pkgs.haskell.lib.linkWithGold
 thisProject.mtg-scryfall;

thisEnvironment = pkgs.haskell.lib.shellAware 
 thisDerivation;

in
########################################

thisEnvironment

########################################
/* NOTES

====================

[e.g.] 

nix-shell --argstr compilerName   ghc843
nix-shell --argstr compilerFlavor integer-simple
nix-shell --arg    withProfiling  false

====================

#TODO attribute 'buildInputs' of the derivation interactive-environment cannot coerce a function to a string

====================

nix-repl> builtins.baseNameOf ~/haskell/spiros
"spiros"

nix-repl> builtins.baseNameOf "~/haskell/spiros"
"spiros"

nix-repl> builtins.baseNameOf "https://github.com/sboosali/spiros"
"spiros"

====================

let

isSourceFile = path: t:
                baseNameOf p != "result"
                    && baseNameOf p != ".git"
                    && baseNameOf p != ".stack-work";

 nixpkgs.stdenv.mkDerivation {
    src = builtins.filterSource isSourceFile p;
    ...
 }

====================

e.g. isCabalNewBuildSourceFile

  nix-repl> builtins.match "\.ghc.environment\..*" ".ghc.environment.x86_64-linux-8.2.2"    
  [ ]
  
  nix-repl> builtins.match "\.ghc.environment\..*" "spiros.cabal"     
  null

====================

builtins.filterSource 

given
  
  builtins.filterSource <PREDICATE> <SOURCE>

the binary-predicate <PREDICATE> is called on each file *and* directory (and symlink) recursively-contained within <SOURCE>.

====================

*/
########################################