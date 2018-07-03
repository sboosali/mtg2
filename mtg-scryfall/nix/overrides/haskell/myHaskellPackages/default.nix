dependencies@
{ lib
, fetchgit
}:

haskellPackages:

########################################
let

#TODO refactor: currently `haskellPackages` must be manually guaranteed to be consistent across `utilities` and `allPackages`.


newHaskellPackages = haskellPackages.override {
 overrides = self: super: 

   let 
   #TODO haskell-override file

   utilities     = (import ./utilities) 
                   (dependencies // { 
                     haskellPackages = self; 
                   })
                   ;

   extraPackages = (import ./all)
                   { inherit lib utilities;
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