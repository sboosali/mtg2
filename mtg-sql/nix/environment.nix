##################################################
{ stdenv
, systemPackages
, postgresqlPackages
}:

##################################################
# Imports ########################################
##################################################
let
#------------------------------------------------#

utilities = import ./utilities.nix {};

#------------------------------------------------#

programs = import ./programs.nix
  { inherit systemPackages postgresqlPackages;
  };

#------------------------------------------------#

variables = utilities.asShellEnvironment (import ./variables.nix
  {
  });

#------------------------------------------------#
in
##################################################

stdenv.mkDerivation {

  name = "mtg-sql_environment";

  description = ''
  Environment for the MTG SQL database application.
  '';

  #----------------------------#

  # The packages in the « buildInputs » list
  # will be added to the « PATH » in our shell.

  buildInputs = programs;

  shellHook = ''
    # 

    export PGDATA=${variables.PGDATA}
    mkdir -p $PGDATA

    export LC_ALL=${variables.LC_ALL}
    export LC_MESSAGES=${variables.LC_MESSAGES}

    # 

    initdb

    # 
  '';

  #----------------------------#

   # « installPhase » is « make install » by default.

   installPhase = ''

     mkdir -p $out/

     InstallDirectory=$out make install
   '';

  #----------------------------#

}
##################################################