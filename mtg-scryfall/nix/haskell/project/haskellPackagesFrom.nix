haskellCompiler@
{ name   ? null
, flavor ? null
}:

pkgs:

########################################

/*

  (import ./<this-file>.nix)
    :: { <systemPackages> } 
    -> { name :: Maybe String; flavor :: Maybe String }
    -> { <haskellPackages> }

*/

########################################

if      haskellCompiler.name == null
then    pkgs.haskellPackages

else if haskellCompiler.flavor == null
then    pkgs.haskell.packages.${haskellCompiler.name}

else    pkgs.haskell.packages.${haskellCompiler.flavor}.${haskellCompiler.name}

########################################
/* NOTES

e.g.

  > (import ./packages.nix) pkgs {}
  pkgs.haskellPackages

  > (import ./packages.nix) pkgs { name = null; flavor = null; }
  pkgs.haskellPackages

  > (import ./packages.nix) pkgs { name = "ghc843"; }
  pkgs.haskell.packages.ghc843

  > (import ./packages.nix) pkgs { name = "ghc843"; flavor = "integer-simple"; }
  pkgs.haskell.packages.integer-simple.ghc843

*/
########################################