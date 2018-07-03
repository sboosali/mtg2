projectConfiguration@
{ haskellCompiler
, ...
}

self:
super:

########################################
let

haskellCompilerConfiguration = haskellCompiler;

haskellCompiler' =
 (import ./original.nix) haskellCompilerConfiguration self super;

in
########################################

haskellCompiler'

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