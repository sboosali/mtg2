--------------------------------------------------

{-| The @mtg-json@ executable.

* 'Program.MTG.JSON.CLI': the command-line options (parsing, defaulting, etc). See 'getCommand'.
* 'Program.MTG.JSON.IO': the program effects (the logic, filesystem interaction, etc). See 'runCommand'.

-}

module Main ( main ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import Program.MTG.JSON.CLI       (getCommand)
import Program.MTG.JSON.IO        (runCommand)

import Program.MTG.JSON.Prelude

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

{-| the @main@ procedure of the @skeletor-haskell@ executable.

@≡ 'getCommand' >>= 'runCommand'
@

-}

main :: IO ()
main = do

  command <- getCommand

  runCommand command

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------