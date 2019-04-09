--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------

{- | 

-}

module MTG.SQL.Postgres.Enum where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.SQL.Postgres.Prelude

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc as PP
import           "prettyprinter" Data.Text.Prettyprint.Doc ( (<+>) )

import qualified "prettyprinter-ansi-terminal" Data.Text.Prettyprint.Doc.Render.Terminal as PP.ANSI

--------------------------------------------------

import qualified "base" Data.List as List

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type SQLDoc = PP.Doc (Maybe SQLAnnotation)

--------------------------------------------------

type ANSIDoc = PP.Doc PP.ANSI.AnsiStyle

--------------------------------------------------

data SQLCreateEnum = SQLCreateEnum

  { enumName   :: String
  , enumLabels :: [String]
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

data SQLAnnotation

  = SQLKeyword 
  | SQLString 
  | SQLType
  | SQLLiteral
  | SQLComment

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------
-- Annotations...

--------------------------------------------------

reAnnotate_SQL_ANSI :: SQLDoc -> ANSIDoc
reAnnotate_SQL_ANSI = reAnnotate_SQL_ANSI go

  where

  go = maybe mempty interpretSQLAnnotationsAsANSIColors

--------------------------------------------------

interpretSQLAnnotationsAsANSIColors :: SQLAnnotation -> PP.ANSI.AnsiStyle
interpretSQLAnnotationsAsANSIColors = \case

   SQLKeyword -> PP.ANSI.color PP.ANSI.Magenta
   SQLString  -> PP.ANSI.color PP.ANSI.Green <> PP.ANSI.underlined
   SQLType -> PP.ANSI.color PP.ANSI.Blue <> PP.ANSI.bold
   SQLLiteral -> PP.ANSI.color PP.ANSI.Green
   SQLComment  -> PP.ANSI.color PP.ANSI.Yellow <> PP.ANSI.italicized

--------------------------------------------------

annotateKeyword :: String -> SQLDoc
annotateKeyword s = PP.annotate (Just SQLKeyword) (PP.pretty s)

annotateString :: String -> SQLDoc
annotateString s = PP.annotate (Just SQLString) (PP.pretty s)

annotateType :: String -> SQLDoc
annotateType s = PP.annotate (Just SQLType) (PP.pretty s)

annotateComment :: String -> SQLDoc
annotateComment s = PP.annotate (Just SQLComment) (PP.pretty s)

annotateLiteral :: String -> SQLDoc
annotateLiteral s = PP.annotate (Just SQLLiteral) (PP.pretty s)

--------------------------------------------------
-- « CREATE TYPE AS ENUM »...

sql_CREATE_TYPE_AS_ENUM :: SQLCreateEnum -> PP.SimpleDocStream

sql_CREATE_TYPE_AS_ENUM SQLCreateEnum{ enumName, enumLabels } = PP. doc
  where

  doc = ppSqlEnumLabels enumName enumLabels

--------------------------------------------------

ppSqlEnumLabels :: String -> [String] -> SQLDoc
ppSqlEnumLabels enumName enumLabels = PP.hsep

  [ annotateKeyword "CREATE TYPE"
  , docName 
  , annotateKeyword "AS ENUM"
  , docLabels1
  ]

  where

  docName = annotateType enumName

  docLabels1 = PP.align (docLabels0 <> PP.semi)
  docLabels0 = ppSqlEnumLabels enumLabels

--------------------------------------------------

ppSqlEnumLabels :: [String] -> SQLDoc
ppSqlEnumLabels enumLabels = tupled docLabels
  where

  docLabels = ppSqlEnumLabel `map` enumLabels

--------------------------------------------------

ppSqlEnumLabel :: String -> SQLDoc
ppSqlEnumLabel enumLabel = docLabel1
  where

   docLabel1 = annotateString docLabel0
   docLabel0 = PP.squotes (from enumLabel)

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- | Like 'PP.tupled', but for (JavaScript-style) trailing commas, not (Haskell-style) leading commas.

tupled :: [PP.Doc i] -> SQLDoc
tupled 

  = PP.punctuate PP.comma
  > parenthesize
  > PP.group

--------------------------------------------------

parenthesize = PP.enclose PP. leftParenthesis rightParenthesis

leftParenthesis = PP.flatAlt "( " "("

rightParenthesis = PP.flatAlt " )" ")"

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------