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
  putStrLn "-- DocTest: Library --------------------"
  putStrLn "----------------------------------------"

  doctestLib

  putStrLn "----------------------------------------"
  putStrLn "-- DocTest: Executable -----------------"
  putStrLn "----------------------------------------"

  --doctestExe

  putStrLn "----------------------------------------"

--------------------------------------------------

doctestLib :: IO ()
doctestLib = doctest (sourcesLib ++ flagsLib)

--------------------------------------------------

doctestExe :: IO ()
doctestExe = doctest (sourcesExe ++ flagsExe)

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

sourcesLib = modules2filepaths "hs" "library" $
  "MTG.Types MTG.Types.Prelude MTG.Classes.Prelude MTG.Classes.Print MTG.Classes.Parse MTG.Types.Errors MTG.Text.List.Colors MTG.Text.List.ManaCost MTG.Number.CMC MTG.Enum.Color \
  \ MTG.Text.Artist MTG.Text.Block MTG.Text.Border MTG.Text.Cardtype MTG.Text.Color MTG.Text.Edition MTG.Text.Format MTG.Text.Frame MTG.Text.Keyword MTG.Text.Language MTG.Text.Layout MTG.Text.Legality MTG.Text.ManaSymbol MTG.Text.Name MTG.Text.Rarity MTG.Text.Subtype MTG.Text.Supertype MTG.Text.Symbol MTG.Text.Watermark"

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

  [ "-fdefer-type-errors"
  ]

--------------------------------------------------

optionsExe :: [String]
optionsExe =

  [ "-i" <> "./library"
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