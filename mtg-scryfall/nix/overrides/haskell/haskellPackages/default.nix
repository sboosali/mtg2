projectConfiguration@
{ haskellCompiler  # :: { name :: Maybe String; flavor :: Maybe String }
, haskellFlags     # :: { <key> :: Bool; <key> :: Bool; ... }
}:

pkgs:

########################################
let

/*
   :: { name :: Maybe String; flavor :: Maybe String }
   -> { <haskellPackages> }
*/
byCompiler = {name,flavor}:
 (import ./original.nix) { inherit name flavor; } pkgs;

# :: { <haskellPackages> } -> { <haskellPackages> }
withProfiling =
 if   haskellFlags.profiling
 then (import ./mkderivation/profiling.nix)
 else pkgs.lib.id;

# :: { <haskellPackages> } -> { <haskellPackages> }
withHoogle =
 if   haskellFlags.hoogle
 then (import ./mkderivation/hoogle.nix)
 else  pkgs.lib.id;

in
########################################
let

haskellPackagesWithCompiler =
 byCompiler haskellCompiler;

haskellPackagesWithProfiling = 
 withProfiling haskellPackagesWithCompiler;

haskellPackagesWithHoogle =
 withHoogle haskellPackagesWithProfiling;

in
########################################

haskellPackagesWithHoogle

########################################