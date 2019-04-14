--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE PatternSynonyms            #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-| 'Language' is a natural language in which /Magic: The Gathering/ cards have been printed.

Languages that are officially supported:

* 'English'
* 'German'
* 'French'
* 'Italian'
* 'Spanish'
* 'Portuguese'
* 'Japanese'
* 'Chinese'
* 'Russian'
* 'Taiwanese'
* 'Korean'

== Types

* `Language`
* `LanguageInfo`

== Examples

>>> greekInfo <- parseLanguageInfo "Ελληνικά (gr)"
>>> greekInfo
LanguageInfo {abbreviation = "gr", endonym = "Ελληνικά"}
>>> prettyLanguageInfo greekInfo
Ελληνικά (gr)

-}

module MTG.Text.Language where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--------------------------------------------------

import "lens" Control.Lens (makeLenses, makePrisms)

--------------------------------------------------

--import qualified "parsers" Text.Parser.Combinators as P
import qualified "parsers" Text.Parser.Char        as P
import qualified "parsers" Text.Parser.Token       as P

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc as PP
import           "prettyprinter" Data.Text.Prettyprint.Doc ( (<+>) )

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ((%))

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "text" Data.Text as Text

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

data LanguageInfo = LanguageInfo

  { abbreviation :: Text
  , endonym      :: Text
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)

  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'englishInfo'@
instance Default LanguageInfo where def = englishInfo

--------------------------------------------------
--------------------------------------------------

newtype Language = Language Text

  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @= 'English'@
instance Default Language where def = English

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

knownLanguages :: [Language]
knownLanguages =

  [ English
  , German
  , French
  , Italian
  , Spanish
  , Portuguese
  , Japanese
  , Chinese
  , Russian
  , Taiwanese
  , Korean
  ]

--------------------------------------------------

-- | @≡ 'languageInfo' ... 'knownLanguages'@

knownLanguageInfo :: [LanguageInfo]
knownLanguageInfo = languageInfo `concatMap` knownLanguages

--------------------------------------------------

-- | 'englishAbbreviation' and 'englishEndonym'. 
englishInfo :: LanguageInfo
englishInfo = LanguageInfo englishAbbreviation englishEndonym

-- | 'germanAbbreviation' and 'germanEndonym'. 
germanInfo :: LanguageInfo
germanInfo = LanguageInfo germanAbbreviation germanEndonym

-- | 'frenchAbbreviation' and 'frenchEndonym'. 
frenchInfo :: LanguageInfo
frenchInfo = LanguageInfo frenchAbbreviation frenchEndonym

-- | 'italianAbbreviation' and 'italianEndonym'. 
italianInfo :: LanguageInfo
italianInfo = LanguageInfo italianAbbreviation italianEndonym

-- | 'spanishAbbreviation' and 'spanishEndonym'. 
spanishInfo :: LanguageInfo
spanishInfo = LanguageInfo spanishAbbreviation spanishEndonym

-- | 'portugueseAbbreviation' and 'portugueseEndonym'. 
portugueseInfo :: LanguageInfo
portugueseInfo = LanguageInfo portugueseAbbreviation portugueseEndonym

-- | 'japaneseAbbreviation' and 'japaneseEndonym'. 
japaneseInfo :: LanguageInfo
japaneseInfo = LanguageInfo japaneseAbbreviation japaneseEndonym

-- | 'chineseAbbreviation' and 'chineseEndonym'. 
chineseInfo :: LanguageInfo
chineseInfo = LanguageInfo chineseAbbreviation chineseEndonym

-- | 'russianAbbreviation' and 'russianEndonym'. 
russianInfo :: LanguageInfo
russianInfo = LanguageInfo russianAbbreviation russianEndonym

-- | 'taiwaneseAbbreviation' and 'taiwaneseEndonym'. 
taiwaneseInfo :: LanguageInfo
taiwaneseInfo = LanguageInfo taiwaneseAbbreviation taiwaneseEndonym

-- | 'koreanAbbreviation' and 'koreanEndonym'. 
koreanInfo :: LanguageInfo
koreanInfo = LanguageInfo koreanAbbreviation koreanEndonym

--------------------------------------------------

pattern English :: Language
pattern English = Language "English"

pattern German :: Language
pattern German = Language "German"

pattern French :: Language
pattern French = Language "French"

pattern Italian :: Language
pattern Italian = Language "Italian"

pattern Spanish :: Language
pattern Spanish = Language "Spanish"

pattern Portuguese :: Language
pattern Portuguese = Language "Portuguese"

pattern Japanese :: Language
pattern Japanese = Language "Japanese"

pattern Chinese :: Language
pattern Chinese = Language "Chinese"

pattern Russian :: Language
pattern Russian = Language "Russian"

pattern Taiwanese :: Language
pattern Taiwanese = Language "Taiwanese"

pattern Korean :: Language
pattern Korean = Language "Korean"

--------------------------------------------------

englishAbbreviation :: Text
englishAbbreviation = "en"

germanAbbreviation :: Text
germanAbbreviation = "de"

frenchAbbreviation :: Text
frenchAbbreviation = "fr"

italianAbbreviation :: Text
italianAbbreviation = "it"

spanishAbbreviation :: Text
spanishAbbreviation = "es"

portugueseAbbreviation :: Text
portugueseAbbreviation = "pt"

japaneseAbbreviation :: Text
japaneseAbbreviation = "jp"

chineseAbbreviation :: Text
chineseAbbreviation = "cn"

russianAbbreviation :: Text
russianAbbreviation = "ru"

taiwaneseAbbreviation :: Text
taiwaneseAbbreviation = "tw"

koreanAbbreviation :: Text
koreanAbbreviation = "ko"

--------------------------------------------------

englishEndonym :: Text
englishEndonym = "English"

germanEndonym :: Text
germanEndonym = "Deutsch"

frenchEndonym :: Text
frenchEndonym = "Français"

italianEndonym :: Text
italianEndonym = "Italiano"

spanishEndonym :: Text
spanishEndonym = "Español"

portugueseEndonym :: Text
portugueseEndonym = "Português"

japaneseEndonym :: Text
japaneseEndonym = "日本語"

chineseEndonym :: Text
chineseEndonym = "简体中文"

russianEndonym :: Text
russianEndonym = "Русский"

taiwaneseEndonym :: Text
taiwaneseEndonym = "繁體中文"

koreanEndonym :: Text
koreanEndonym = "한국어"

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | Information about the given language.

== Examples

>>> :set -XOverloadedStrings
>>> pretty (languageInfo "Russian" :: Maybe LanguageInfo)
Русский (ru)
>>> (languageInfo "Greek" :: Maybe LanguageInfo)
Nothing

-}


languageInfo :: (MonadThrow m) => Language -> m LanguageInfo
languageInfo = \case  

  Language "English"    -> return englishInfo
  Language "German"     -> return germanInfo
  Language "French"     -> return frenchInfo
  Language "Italian"    -> return italianInfo
  Language "Spanish"    -> return spanishInfo
  Language "Portuguese" -> return portugueseInfo
  Language "Japanese"   -> return japaneseInfo
  Language "Chinese"    -> return chineseInfo
  Language "Russian"    -> return russianInfo
  Language "Taiwanese"  -> return taiwaneseInfo
  Language "Korean"     -> return koreanInfo

  Language language     ->

    let s = (runFormat ("``` " % Format.string % " ```: unknown language: {{{ languageInfo " % Format.string % " }}}: ")
             (displayName 'languageInfo)
             (showWithApplicationPrecedence language)
            )
    in errorM s

--------------------------------------------------

{- | @≡ 'ppLanguageInfo' -}

prettyLanguageInfo :: LanguageInfo -> String 
prettyLanguageInfo = runPrinter ppLanguageInfo

--------------------------------------------------

{- | @≡ 'pLanguageInfo'

== Example

>>> parseLanguageInfo "Ελληνικά (gr)"
LanguageInfo {abbreviation = "gr", endonym = "Ελληνικά"}

-}

parseLanguageInfo :: (MonadThrow m) => String -> m LanguageInfo
parseLanguageInfo = runParser 'pLanguageInfo pLanguageInfo

--------------------------------------------------
-- Pretty ----------------------------------------
--------------------------------------------------

-- | @≡ 'ppLanguageInfo'@

instance Pretty LanguageInfo where

  pretty = ppLanguageInfo

--------------------------------------------------

{- | Pretty-Print 'LanguageInfo'.

== Example

>>> pretty chineseInfo
简体中文 (cn)

-}

ppLanguageInfo :: LanguageInfo -> Doc i
ppLanguageInfo LanguageInfo{ abbreviation, endonym } =

  pretty endonym <+> PP.parens (pretty abbreviation)

--------------------------------------------------
-- Parse -----------------------------------------
--------------------------------------------------

-- | @≡ 'pLanguageInfo'@

instance Parse LanguageInfo where

  parser = pLanguageInfo

--------------------------------------------------

{- | Parse 'LanguageInfo'.

Inverts 'ppLanguageInfo'.

== Example

>>> parseLanguageInfo "Ελληνικά (gr)"
LanguageInfo {abbreviation = "gr", endonym = "Ελληνικά"}

-}

pLanguageInfo :: (MTGParsing m) => m LanguageInfo
pLanguageInfo = p <?> "LanguageInfo"
  where

  p = do

      endonym <- pLanguageEndonym
      P.spaces
      abbreviation <- P.parens pLanguageAbbreviation

      return LanguageInfo{..}

--------------------------------------------------

{- | Parse an 'endonym'.

== Example

>>> runParser 'pLanguageEndonym "words before ( words after"
"words before"

-}

pLanguageEndonym :: (MTGParsing m) => m Text
pLanguageEndonym = pUnfreeText "("

--------------------------------------------------

{- | Parse an 'abbreviation'.

== Example

>>> runParser 'pLanguageAbbreviation " GR "
"gr"

-}

pLanguageAbbreviation :: (MTGParsing m) => m Text
pLanguageAbbreviation = go <$> p <?> "Abbreviation"
  where

  go = Text.pack > Text.toLower > Text.strip

  p = some P.alphaNum

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''Language

makeLenses ''LanguageInfo

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------