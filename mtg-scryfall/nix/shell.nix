{ nixpkgs        ? import ./nixpkgs {}

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

####################

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

haskell =
 (import ./overrides/haskell) projectConfiguration self super;

####################

packageParameters = {
  haskellPackages = haskell.packages;
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



====================

*/
########################################