##################################################
{ nixpkgs ? (import ./nixpkgs.nix)
, pkgs    ? (import nixpkgs {}).pkgs
}:

##################################################
# Imports ########################################
##################################################
let
#------------------------------------------------#

environment = import ./environment.nix
  {
    inherit (pkgs) stdenv postgresqlPackages;
    systemPackages = pkgs;
  };

#------------------------------------------------#
in
##################################################

environment

##################################################