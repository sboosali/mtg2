haskellPackages:

########################################
let

haskellPackagesWithTesting = haskellPackages.override
 {
   overrides = self: super:
   {
     mkDerivation = args:
       super.mkDerivation (args //
         {
           doCheck = true;
         });
   };
 };

in
########################################

haskellPackagesWithTesting

########################################