haskellPackages:

########################################
let

haskellPackagesWithBenchmarking = haskellPackages.override
 {
   overrides = self: super:
   {
     mkDerivation = args:
       super.mkDerivation (args //
         {
           doBench = true;
         });
   };
 };

in
########################################

haskellPackagesWithBenchmarking

########################################