{ callPackage
, callHackage
, callCabal
#TODO , callCabalProject
, callPrefetched
, ...
}:

########################################
let

in
########################################
{

    spiros =
      callCabal { path = ~/haskell/spiros; }; #TODO relative-path
    
    reflex =
      callPrefetched ./haskell/myHaskellPackages/git/reflex.json; #TODO shorten path to ./overrides/git/spiros.json

    # spiros = self.callCabal2nix "spiros" 
    #           (fetchPrefetched ./overrides/haskell/myHaskellPackages/git/spiros.json) {}; 

    # reflex = self.callCabal2nix "reflex"
    #           (fetchPrefetched ./overrides/haskell/myHaskellPackages/git/reflex.json) {}; 
}
########################################