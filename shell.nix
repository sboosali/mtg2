# import ./mtg-scryfall/shell.nix

(import ./.).shellFor {
  packages = p: [p.mtg-scryfall ];
  withHoogle = true;
}

