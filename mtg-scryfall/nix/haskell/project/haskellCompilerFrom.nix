haskellCompiler@
{ name   ? null
, flavor ? null
}:

self:
super:

/* 
   :: { <systemPackages> } 
   -> { name :: Maybe String; flavor :: Maybe String }
   -> { <derivation> }
*/

########################################

if      haskellCompiler.name == null
then    (if      builtins.hasAttr "haskellCompiler" self
         then    self.haskellCompiler
         #
         else if builtins.hasAttr "ghc" self.compiler
         then    self.haskell.compiler.ghc
         #
         else    self.haskell.compiler.ghc843)

else if haskellCompiler.flavor == null
then    self.haskell.compiler.${haskellCompiler.name}

else    self.haskell.compiler.${haskellCompiler.flavor}.${haskellCompiler.name}

#TODO ^ these conditionals aren't complete.

########################################
/* NOTES

e.g.

  > (import ./compiler) pkgs _ {}
  pkgs.haskellCompiler

  > (import ./compiler) pkgs _ { name = null; flavor = null; }
  pkgs.haskellCompiler

  > (import ./compiler) pkgs _ { name = "ghc843"; }
  pkgs.haskell.compiler.ghc843

  > (import ./compiler) pkgs _ { name = "ghc843"; flavor = "integer-simple"; }
  pkgs.haskell.compiler.integer-simple.ghc843

*/
########################################