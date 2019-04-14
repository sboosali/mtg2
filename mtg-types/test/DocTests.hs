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
  "MTG.Types MTG.Types.Prelude MTG.Classes.Prelude MTG.Classes.Print MTG.Classes.Parse MTG.Types.Errors MTG.Text.List.Colors MTG.Number.CMC MTG.Enum.Color \
  \ MTG.Text.Artist MTG.Text.Block MTG.Text.Border MTG.Text.Cardtype MTG.Text.Color MTG.Text.Edition MTG.Text.Format MTG.Text.Frame MTG.Text.Keyword MTG.Text.Language MTG.Text.Layout MTG.Text.Legality MTG.Text.ManaSymbol MTG.Text.Name MTG.Text.Rarity MTG.Text.Subtype MTG.Text.Supertype MTG.Text.Symbol MTG.Text.Watermark"

--------------------------------------------------

flags :: [String]
flags = concat

  [ extensions2flags extensions
  , options
  ]

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

  [ "-fdefer-type-errors"
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