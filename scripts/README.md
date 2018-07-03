====================

run these scripts

  ./scripts/*.sh

from the *project root* directory 

  $ cd <...>/mtg/
  $ ./scripts/<...>.sh

i.e. not from the scripts directory, a sub-directory

  <...>/mtg/scripts/

====================

the environment

  ./scripts/shell.nix

should provision the few executables we need for running and developing are simple bash Scripts. i.e. a pure shell should work, like

  $ nix-shell --pure ./scripts/shell.nix --run ./scripts/get-cards-from-scryfall.sh 

====================



====================