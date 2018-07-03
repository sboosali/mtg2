{ nixpkgsFile  ? ./nixpkgs.json

, overlayFiles ? []  # [ ./overlays/default.nix ]
, configFiles  ? []  # [ ./config/default.nix   ]
}:

########################################
let

nixpkgs-bootstrap =
 import <nixpkgs> { };

nixpkgs-pinned-identifier =
 builtins.fromJSON (builtins.readFile nixpkgsFile);

nixpkgs-pinned-source =
 nixpkgs-bootstrap.fetchFromGitHub {
  owner = "NixOS";
  repo  = "nixpkgs";
  inherit (nixpkgs-pinned-identifier) rev sha256;
 };

# :: [ Object -> Object -> Object ]
#
overlays =
 map import overlayFiles;

# :: Object
#
config =
 nixpkgs-bootstrap.lib.foldr (xs: ys: xs // ys) {}
  (map import configFiles);

nixpkgs-pinned =
 import nixpkgs-pinned-source { inherit overlays config; };

in
########################################

nixpkgs-pinned

########################################
/* NOTES

====================

`nixpkgsFile` may come from `nix-prefetch-git` of the official GitHub repository. e.g.:

  $ nix-prefetch-git https://github.com/NixOS/nixpkgs.git > nixpkgs.json

====================

for `overlayFiles` and `configFiles`, later items in the list take precedence;
i.e. the later files' attrs shadow the sooner ones;
e.g. in `overlayFiles = [ ./overlay1.nix ./overlay2.nix ]`, for attributes in both `./overlay1.nix` and `./overlay2.nix`, only the attributes of `./overlay2.nix` are kept.

====================

`foldr (//) {}` is right-biased, because `(//)` is right-biased; a.k.a.
duplicate keys (with conflicting values) resolve to the last item (i.e. the last item's key's value).

====================

  { inherit overlays config; };

====================

*/
########################################