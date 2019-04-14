{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

Languages that are officially supported:

* 'english'
* 'german'
* 'french'
* 'italian'
* 'spanish'
* 'portuguese'
* 'japanese'
* 'chinese'
* 'russian'
* 'taiwanese'
* 'korean'

-}

module MTG.Text.Language where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--------------------------------------------------

import "lens" Control.Lens (makeLenses, makePrisms)

--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc as PP
import           "prettyprinter" Data.Text.Prettyprint.Doc ( (<+>) )

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

newtype Language = Language Text

  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

-- | @= 'english'@
instance Default Language where def = english

--------------------------------------------------
--------------------------------------------------

data LanguageInfo = LanguageInfo

  { _languageAbbreviation :: Text
  , _languageEndonym      :: Text
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

-- | @= 'englishInfo'@
instance Default LanguageInfo where def = englishInfo

--------------------------------------------------

{- |
>>> pretty chineseInfo

-}

instance Pretty LanguageInfo where
  pretty LanguageInfo{ _languageAbbreviation, _languageEndonym } = pretty _languageEndonym <+> PP.parens (pretty _languageAbbreviation )

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

knownLanguages :: [Language]
knownLanguages =

  [ english
  , german
  , french
  , italian
  , spanish
  , portuguese
  , japanese
  , chinese
  , russian
  , taiwanese
  , korean
  ]

--------------------------------------------------

-- | @≡ fmap 'languageInfo' 'knownLanguages'@
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

english :: Language
english = "English"

german :: Language
german = "German"

french :: Language
french = "French"

italian :: Language
italian = "Italian"

spanish :: Language
spanish = "Spanish"

portuguese :: Language
portuguese = "Portuguese"

japanese :: Language
japanese = "Japanese"

chinese :: Language
chinese = "Chinese"

russian :: Language
russian = "Russian"

taiwanese :: Language
taiwanese = "Taiwanese"

korean :: Language
korean = "Korean"

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

{- | 


== Examples

>>> :set -XOverloadedStrings
>>> pretty (languageInfo "Russian" :: Maybe LanguageInfo)
Русский (ru)

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
  Language language     -> errorM ("<<< languageInfo _ >>> unknown language: " <> show language)

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