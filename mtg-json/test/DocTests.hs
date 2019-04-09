
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

main = do

  printDivider

  doctest (sources ++ flags)

  printDivider

--------------------------------------------------

sources = modules2filepaths "hs" "library" $
  "MTG.SQL.Postgres MTG.SQL.Postgres.Enum"

--------------------------------------------------

flags = (extensions2flags extensions) ++ options

--------------------------------------------------

extensions :: [String]
extensions =

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

options :: [String]
options =

  [ 
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
modules2filepaths extension directory = fmap go . words

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

$ grep -h -i LANGUAGE -r library/ | sort | uniq | ...

$ cat ... | sort | uniq | xargs | sed -e 's/ / /g'

-}

-- [1] every module in this directory (i.e. `hs-source-dirs`),
-- [2] and all language extensions,
-- whether enabled by default or otherwise used,
-- (i.e. both `default-extensions` and `other-extensions`)
-- EXCEPT those that conflict
-- (e.g. DeriveAnyClass and GeneralizedNewtypeDeriving)

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------