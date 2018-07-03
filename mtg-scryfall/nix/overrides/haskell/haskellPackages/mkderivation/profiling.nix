haskellPackages:

########################################

haskellPackages.override
 {
   overrides = self: super:
   {
     mkDerivation = args:
       super.mkDerivation (args //
         {
           enableLibraryProfiling = true;
         });
   };
 }

########################################