{ nixpkgs ? import <nixpkgs> {}
}:
########################################
let

systemTools = with nixpkgs; [

  curl

];

haskellTools = with nixpkgs.haskellPackages; [

  ShellCheck

];

in
########################################

nixpkgs.buildEnv {
 name  = "mtg-scripts";
 paths = systemTools ++ haskellTools;
}
########################################
