##################################################
{ stdenv
, systemPackages
, nodePackages
}:

##################################################
# Utilities ######################################
##################################################
let
#------------------------------------------------#

asShellEnvironment = attrs:

  builtins.mapAttrs asShellVariable attrs;

#------------------------------------------------#

asShellVariable = k: v:

  builtins.toString v;

#------------------------------------------------#
in
##################################################
# Imports ########################################
##################################################
let
#------------------------------------------------#

programs = import ./programs.nix
  { inherit systemPackages nodePackages;
  };

#------------------------------------------------#

variables = asShellEnvironment (import ./variables.nix
  {
  });

#------------------------------------------------#
in
##################################################

stdenv.mkDerivation {

  name = "sboosali.github.io_environment";

  description = ''
  Programs for web development:

  • HTML/CSS/JS (+ Bash/JSON): Linters.
  • JS: Package Manager, Build Tool, Project Scaffolding.
  • JS: Interpreter, Minifier, Type System.
  '';

  #----------------------------#

  # The packages in the « buildInputs » list
  # will be added to the « PATH » in our shell.

  buildInputs = programs;

  shellHook = ''
    export NODE_PATH=${variables.NODE_PATH}
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