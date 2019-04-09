--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ApplicativeDo         #-}

--------------------------------------------------
--------------------------------------------------

{-| -}

module Main where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.CSV

import Program.MTG.CSV.Prelude

--------------------------------------------------

import qualified "base" Control.Exception   as E
import qualified "base" System.Environment  as IO

import           "base" System.Exit
import           "base" Data.Foldable
import           "base" Data.Monoid

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

{-| 

-}

main :: IO ()
main = do

  IO.getArgs >>= mainWith

--------------------------------------------------

{-| 

-}

mainWith :: [String] -> IO ()
mainWith arguments = do

  putStrLn "\n----------------------------------------\n"

  putStrLn `traverse_` arguments

  putStrLn "\n----------------------------------------\n"

  

  return ()

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------