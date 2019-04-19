--------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "doctest" Test.DocTest

--------------------------------------------------

import "base" Data.Char
import "base" Data.Foldable
import "base" Prelude

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

main :: IO ()
main = do

  putStrLn "----------------------------------------"
  putStrLn "-- DocTest: Librarie -------------------"
  putStrLn "----------------------------------------"

  doctestLib1

  doctestLib2

  putStrLn "----------------------------------------"
  putStrLn "-- DocTest: Executables ----------------"
  putStrLn "----------------------------------------"

--  doctestExe

  putStrLn "----------------------------------------"

--------------------------------------------------

doctestLib1 :: IO ()
doctestLib1 = doctest (sourcesLib1 ++ flagsLib)

--------------------------------------------------

doctestLib2 :: IO ()
doctestLib2 = doctest (sourcesLib2 ++ flagsLib)

--------------------------------------------------

doctestExe :: IO ()
doctestExe = doctest (sourcesExe ++ flagsExe)

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

sourcesLib1 = modules2filepaths "hs" "library" $

  "MTG.JSON MTG.JSON.Prelude MTG.JSON.UUID MTG.JSON.Schema.Set MTG.JSON.Schema.Card"

--------------------------------------------------

sourcesLib2 = modules2filepaths "hs" "srcdst" $

  "Prelude.SrcDst Data.SrcDst Conduit.SrcDst"

--------------------------------------------------

sourcesExe = modules2filepaths "hs" "executables" $

  "Program.MTG.JSON.Types Program.MTG.JSON.IO Program.MTG.JSON.CLI Program.MTG.JSON.Constants Program.MTG.JSON.Prelude"

--------------------------------------------------
--------------------------------------------------

flagsLib :: [String]
flagsLib = concat

  [ extensions2flags extensions0
  , optionsLib
  ]

--------------------------------------------------

flagsExe :: [String]
flagsExe = concat

  [ extensions2flags extensions0
  , optionsExe
  ]

--------------------------------------------------
--------------------------------------------------

optionsLib :: [String]
optionsLib =

  [ -- "-fdefer-type-errors"
  ]

--------------------------------------------------

optionsExe :: [String]
optionsExe =

  [ "-i" <> "./library"
  , "-i" <> "./srcdst"
  , "-i" <> "./executables"
  ]

--------------------------------------------------
--------------------------------------------------

extensions0 :: [String]
extensions0 =

  [ "AutoDeriveTypeable"
  , "BangPatterns"
  , "CPP"
  , "ConstraintKinds"
  , "DataKinds"
  , "DefaultSignatures"
  , "DeriveAnyClass"
  , "DeriveDataTypeable"
  , "DeriveFoldable"
  , "DeriveFunctor"
  , "DeriveGeneric"
  , "DeriveLift"
  , "DeriveTraversable"
  , "DerivingStrategies"
  , "DoAndIfThenElse"
  , "DuplicateRecordFields"
  , "EmptyCase"
  , "EmptyDataDecls"
  , "ExplicitNamespaces"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "FunctionalDependencies"
  , "GADTs"
  , "GeneralizedNewtypeDeriving"
  , "InstanceSigs"
  , "KindSignatures"
  , "LambdaCase"
  , "MultiParamTypeClasses"
  , "MultiWayIf"
  , "NamedFieldPuns"
  , "NoImplicitPrelude"
  , "PackageImports"
  , "PatternSynonyms"
  , "PostfixOperators"
  , "RankNTypes"
  , "RecordWildCards"
  , "ScopedTypeVariables"
  , "StandaloneDeriving"
  , "TupleSections"
  , "TypeFamilies"
  , "TypeOperators"
  , "UndecidableInstances"
  , "ViewPatterns"
  ]

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

putStringsLine :: [String] -> IO ()
putStringsLine = fmap (const ()) . traverse putStrLn

--------------------------------------------------

printDivider :: IO ()
printDivider = putStrLn "----------------------------------------\n"

--------------------------------------------------

printBlank :: IO ()
printBlank = putStrLn ""

--------------------------------------------------

extensions2flags :: [String] -> [String]
extensions2flags = fmap extension2flag . filterBlanks

extension2flag :: String -> String
extension2flag = ("-X" ++)

--------------------------------------------------

modules2filepaths :: String -> String -> String -> [String]
modules2filepaths extension directory

  = fmap go
  . filterBlanks
  . words

  where

  go s = directory ++ "/" ++ (module2filename s) ++ "." ++ extension

--------------------------------------------------

module2filename :: String -> String
module2filename = replace '.' '/'

--------------------------------------------------

replace
  :: (Functor f, Eq a)
  => a -> a -> f a -> f a

replace a b = fmap go
  where

  go c = if c == a then b else c

--------------------------------------------------

filterBlanks :: [String] -> [String]
filterBlanks = filter (not . areAllCharactersBlank)
  where

  areAllCharactersBlank = all isSpace

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------