--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------

{- | Render SQL statements like @CREATE TYPE ... AS ENUM ( ... )@.

== Examples

'ppSQLCreateEnum':

>>> ppSQLCreateEnum SQLCreateEnum{ enumName = "color", enumLabels = [ "white", "blue", "black", "red", "green" ] }
CREATE TYPE color AS ENUM ('white', 'blue', 'black', 'red', 'green');

'sql_CREATE_TYPE_AS_ENUM':

>>> sql_CREATE_color layout = sql_CREATE_TYPE_AS_ENUM layout SQLCreateEnum{ enumName = "color", enumLabels = [ "white", "blue", "black", "red", "green" ] }
>>> Prelude.putStrLn (sql_CREATE_color Nothing)
CREATE TYPE color AS ENUM ('white', 'blue', 'black', 'red', 'green');
>>> Prelude.putStrLn (sql_CREATE_color (Just PP.LayoutOptions { PP.layoutPageWidth = PP.AvailablePerLine 30 1.0 }))
CREATE TYPE color AS ENUM ( 'white',
                            'blue',
                            'black',
                            'red',
                            'green' );


-}

module MTG.SQL.Postgres.Enum where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.SQL.Postgres.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc as PP
--import           "prettyprinter" Data.Text.Prettyprint.Doc ( (<+>) )

import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Render.String as PP.String

--------------------------------------------------

import qualified "prettyprinter-ansi-terminal" Data.Text.Prettyprint.Doc.Render.Terminal as PP.ANSI
--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--import qualified "base" Data.List as List
--import qualified "base" Prelude

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
-- « Doc »s for « CREATE TYPE AS ENUM »...

sql_CREATE_TYPE_AS_ENUM :: Maybe PP.LayoutOptions -> SQLCreateEnum -> String
sql_CREATE_TYPE_AS_ENUM mLayout enum = renderedDocument
  where

  renderedDocument = PP.String.renderString simpleDocument
  simpleDocument   = PP.layoutSmart layout complexDocument
  complexDocument  = ppSQLCreateEnum enum

  layout = mLayout & maybe PP.defaultLayoutOptions id

--------------------------------------------------

ppSQLCreateEnum :: SQLCreateEnum -> SQLDoc
ppSQLCreateEnum SQLCreateEnum{ enumName, enumLabels } = doc
  where

  doc = ppSqlEnumType enumName enumLabels

--------------------------------------------------

ppSqlEnumType :: String -> [String] -> SQLDoc
ppSqlEnumType enumName enumLabels = PP.hsep

  [ annotateKeyword "CREATE TYPE"
  , docName1
  , annotateKeyword "AS ENUM"
  , docLabels1
  ]

  where

  docName1 = annotateType docName0
  docName0 = PP.pretty enumName

  docLabels1 = parenthesize (PP.align docLabels0) <> PP.semi
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
   docLabel0 = PP.squotes (PP.pretty enumLabel)

--------------------------------------------------
-- Annotations...

--------------------------------------------------

reAnnotate_SQL_ANSI :: SQLDoc -> ANSIDoc
reAnnotate_SQL_ANSI = PP.reAnnotate go

  where

  go = maybe mempty interpretSQLAnnotationsAsANSIColors

--------------------------------------------------

interpretSQLAnnotationsAsANSIColors :: SQLAnnotation -> PP.ANSI.AnsiStyle
interpretSQLAnnotationsAsANSIColors = \case

   SQLKeyword -> PP.ANSI.color PP.ANSI.Magenta
   SQLString  -> PP.ANSI.color PP.ANSI.Green   <> PP.ANSI.underlined
   SQLType    -> PP.ANSI.color PP.ANSI.Blue    <> PP.ANSI.bold
   SQLLiteral -> PP.ANSI.color PP.ANSI.Green
   SQLComment -> PP.ANSI.color PP.ANSI.Yellow  <> PP.ANSI.italicized

--------------------------------------------------

annotateKeyword :: SQLDoc -> SQLDoc
annotateKeyword s = PP.annotate (Just SQLKeyword) s

annotateString :: SQLDoc -> SQLDoc
annotateString s = PP.annotate (Just SQLString) s

annotateType :: SQLDoc -> SQLDoc
annotateType s = PP.annotate (Just SQLType) s

annotateComment :: SQLDoc -> SQLDoc
annotateComment s = PP.annotate (Just SQLComment) s

annotateLiteral :: SQLDoc -> SQLDoc
annotateLiteral s = PP.annotate (Just SQLLiteral) s

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

-- | Like 'PP.tupled', but for (JavaScript-style) trailing commas, not (Haskell-style) leading commas.

tupled :: [SQLDoc] -> SQLDoc
tupled 

  = PP.punctuate PP.comma
  > PP.vsep
  > PP.group

--------------------------------------------------

parenthesize :: PP.Doc i -> PP.Doc i
parenthesize = PP.enclose leftParenthesis rightParenthesis

leftParenthesis :: PP.Doc i
leftParenthesis = PP.flatAlt "( " "("

rightParenthesis :: PP.Doc i
rightParenthesis = PP.flatAlt " )" ")"

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------