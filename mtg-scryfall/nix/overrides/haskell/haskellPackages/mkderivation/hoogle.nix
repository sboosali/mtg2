haskellPackages:

########################################

haskellPackages.override
 {
   overrides = self: super:
     {
       ghcWithPackages = self.ghc.withPackages;
       ghc             = super.ghc //
         {
           withPackages = super.ghc.withHoogle;
         };
     };
 }

########################################