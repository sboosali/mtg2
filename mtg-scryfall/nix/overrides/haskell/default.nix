projectConfiguration:

self:
super:

# ^ self/super are global `pkgs`;
# an overlay for `nixpkgs { overlays = [...]; }`.

########################################
let

packages =
 (import ./packages) projectConfiguration self super;

compiler =
 (import ./compiler) projectConfiguration self super;

in
########################################

{ inherit packages compiler;
}

########################################
/* NOTES

====================

====================

*/
########################################