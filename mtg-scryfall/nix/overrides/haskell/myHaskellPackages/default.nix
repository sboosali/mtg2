dependencies@
{ lib
, fetchgit
}:

oldHaskellPackages:

########################################
let

newHaskellPackages = oldHaskellPackages.override {
 overrides = self: super: 

   let 

   extraPackages = (import ./all)
                   { inherit lib fetchgit;
                   }
                   self
                   ;

   in

   extraPackages;
};

in
########################################

newHaskellPackages

########################################
/* NOTES

====================

====================

*/
########################################