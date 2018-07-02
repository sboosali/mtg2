{

 spiros.file = ~/haskell/spiros;
 # absolute or relative.
 # filepath/directory, or nix object.

 Cabal.hackage = "2.3.0.0"; 
 # a numeric string.
 # "2.3.0.0-1"    for revisions.
 # ["2.3.0.0" 1] for revisions.

 reflex.git = ./reflex.json;
 # `nix-prefetch-git`-compatible.
 # json file, or nix object.

}