##################################################
{ systemPackages
, nodePackages
}:

##################################################
let
#------------------------------------------------#

systemPrograms = with systemPackages; [

  nodejs                        # JS Interpreter
  html-tidy                     # HTML Linter
  csslint                       # CSS Linter
  shellcheck                    # Bash Linter

];

#------------------------------------------------#

nodePrograms = with nodePackages; [

  npm                           # JS Package Manager
  webpack                       # JS Build Tool
  webpack-cli                   # JS Build Tool
  eslint                        # JS Linter
  jshint                        # JS Linter
  prettier                      # JS Formatter
  uglify-js                     # JS Minifier
  typescript                    # JS Type System
  jsonlint                      # JSON Linter
  htmlhint                      # HTML Linter
  textlint                      # Markdown Linter
  markdown-link-check           # Markdown Linter
  react-tools                   # React.js
  react-native-cli              # React.js
  create-react-app              # React.js
  create-react-native-app       # React.js

];

#------------------------------------------------#
in
##################################################

nodePrograms ++ systemPrograms

##################################################
## Notes #########################################
##################################################

# html-tidy
# stylelint(?) TODO
# eslint
# shellcheck

##################################################