--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------

{-| 'EditionName' is the name of a /Magic: The Gathering/ edition (a.k.a. a “set”).

== Types

* `EditionName`
* `EditionInfo`

-}

module MTG.Text.Edition where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

import MTG.Text.Language

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "lens" Control.Lens (makeLenses, makePrisms)

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

newtype EditionName = EditionName

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

newtype EditionCode = EditionCode

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

newtype EditionType = EditionType

  Text
 
  deriving stock    (Show,Read)
  deriving stock    (Lift,Data,Generic)

  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (IsString)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

data QualifiedEdition = QualifiedEdition

  { _editionEdition  :: EditionName
  , _editionLanguage :: Maybe Language
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)

  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

data EditionInfo = EditionInfo

  { _editionAbbreviation :: Text
  , _editionDescription  :: Text
--, _editionLanguages    :: [Language] --NOTE a `Set` 
  }

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Lift,Data,Generic)

  deriving anyclass (NFData,Hashable)


--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

pattern E_AL :: EditionCode
pattern E_AL = EditionCode "AL"

pattern E_BE :: EditionCode
pattern E_BE = EditionCode "BE"

pattern E_UN :: EditionCode
pattern E_UN = EditionCode "UN"

pattern E_RV :: EditionCode
pattern E_RV = EditionCode "RV"

pattern E_SUMMER :: EditionCode
pattern E_SUMMER = EditionCode "SUMMER"

pattern E_E4 :: EditionCode
pattern E_E4 = EditionCode "E4"

pattern E_E5 :: EditionCode
pattern E_E5 = EditionCode "E5"

pattern E_E6 :: EditionCode
pattern E_E6 = EditionCode "E6"

pattern E_E7 :: EditionCode
pattern E_E7 = EditionCode "E7"

pattern E_E8 :: EditionCode
pattern E_E8 = EditionCode "E8"

pattern E_E9 :: EditionCode
pattern E_E9 = EditionCode "E9"

pattern E_E10 :: EditionCode
pattern E_E10 = EditionCode "E10"

pattern E_M10 :: EditionCode
pattern E_M10 = EditionCode "M10"

pattern E_M11 :: EditionCode
pattern E_M11 = EditionCode "M11"

pattern E_M12 :: EditionCode
pattern E_M12 = EditionCode "M12"

pattern E_M13 :: EditionCode
pattern E_M13 = EditionCode "M13"

pattern E_M14 :: EditionCode
pattern E_M14 = EditionCode "M14"

pattern E_M15 :: EditionCode
pattern E_M15 = EditionCode "M15"

pattern E_ORI :: EditionCode
pattern E_ORI = EditionCode "ORI"

pattern E_AN :: EditionCode
pattern E_AN = EditionCode "AN"

pattern E_AQ :: EditionCode
pattern E_AQ = EditionCode "AQ"

pattern E_LG :: EditionCode
pattern E_LG = EditionCode "LG"

pattern E_DK :: EditionCode
pattern E_DK = EditionCode "DK"

pattern E_FE :: EditionCode
pattern E_FE = EditionCode "FE"

pattern E_HL :: EditionCode
pattern E_HL = EditionCode "HL"

pattern E_MR :: EditionCode
pattern E_MR = EditionCode "MR"

pattern E_VI :: EditionCode
pattern E_VI = EditionCode "VI"

pattern E_WL :: EditionCode
pattern E_WL = EditionCode "WL"

pattern E_TP :: EditionCode
pattern E_TP = EditionCode "TP"

pattern E_SH :: EditionCode
pattern E_SH = EditionCode "SH"

pattern E_EX :: EditionCode
pattern E_EX = EditionCode "EX"

pattern E_US :: EditionCode
pattern E_US = EditionCode "US"

pattern E_UL :: EditionCode
pattern E_UL = EditionCode "UL"

pattern E_UD :: EditionCode
pattern E_UD = EditionCode "UD"

pattern E_MM :: EditionCode
pattern E_MM = EditionCode "MM"

pattern E_NE :: EditionCode
pattern E_NE = EditionCode "NE"

pattern E_PR :: EditionCode
pattern E_PR = EditionCode "PR"

pattern E_IN :: EditionCode
pattern E_IN = EditionCode "IN"

pattern E_PS :: EditionCode
pattern E_PS = EditionCode "PS"

pattern E_AP :: EditionCode
pattern E_AP = EditionCode "AP"

pattern E_OD :: EditionCode
pattern E_OD = EditionCode "OD"

pattern E_TR :: EditionCode
pattern E_TR = EditionCode "TR"

pattern E_JU :: EditionCode
pattern E_JU = EditionCode "JU"

pattern E_ON :: EditionCode
pattern E_ON = EditionCode "ON"

pattern E_LE :: EditionCode
pattern E_LE = EditionCode "LE"

pattern E_SC :: EditionCode
pattern E_SC = EditionCode "SC"

pattern E_MI :: EditionCode
pattern E_MI = EditionCode "MI"

pattern E_DS :: EditionCode
pattern E_DS = EditionCode "DS"

pattern E_DN5 :: EditionCode
pattern E_DN5 = EditionCode "DN5"

pattern E_CHK :: EditionCode
pattern E_CHK = EditionCode "CHK"

pattern E_BOK :: EditionCode
pattern E_BOK = EditionCode "BOK"

pattern E_SOK :: EditionCode
pattern E_SOK = EditionCode "SOK"

pattern E_RAV :: EditionCode
pattern E_RAV = EditionCode "RAV"

pattern E_GP :: EditionCode
pattern E_GP = EditionCode "GP"

pattern E_DI :: EditionCode
pattern E_DI = EditionCode "DI"

pattern E_IA :: EditionCode
pattern E_IA = EditionCode "IA"

pattern E_AI :: EditionCode
pattern E_AI = EditionCode "AI"

pattern E_CS :: EditionCode
pattern E_CS = EditionCode "CS"

pattern E_TSTS :: EditionCode
pattern E_TSTS = EditionCode "TSTS"

pattern E_TS :: EditionCode
pattern E_TS = EditionCode "TS"

pattern E_PC :: EditionCode
pattern E_PC = EditionCode "PC"

pattern E_FUT :: EditionCode
pattern E_FUT = EditionCode "FUT"

pattern E_LW :: EditionCode
pattern E_LW = EditionCode "LW"

pattern E_MT :: EditionCode
pattern E_MT = EditionCode "MT"

pattern E_SHM :: EditionCode
pattern E_SHM = EditionCode "SHM"

pattern E_EVE :: EditionCode
pattern E_EVE = EditionCode "EVE"

pattern E_ALA :: EditionCode
pattern E_ALA = EditionCode "ALA"

pattern E_CFX :: EditionCode
pattern E_CFX = EditionCode "CFX"

pattern E_ARB :: EditionCode
pattern E_ARB = EditionCode "ARB"

pattern E_ZEN :: EditionCode
pattern E_ZEN = EditionCode "ZEN"

pattern E_WWK :: EditionCode
pattern E_WWK = EditionCode "WWK"

pattern E_ROE :: EditionCode
pattern E_ROE = EditionCode "ROE"

pattern E_SOM :: EditionCode
pattern E_SOM = EditionCode "SOM"

pattern E_MBS :: EditionCode
pattern E_MBS = EditionCode "MBS"

pattern E_NPH :: EditionCode
pattern E_NPH = EditionCode "NPH"

pattern E_ISD :: EditionCode
pattern E_ISD = EditionCode "ISD"

pattern E_DKA :: EditionCode
pattern E_DKA = EditionCode "DKA"

pattern E_AVR :: EditionCode
pattern E_AVR = EditionCode "AVR"

pattern E_RTR :: EditionCode
pattern E_RTR = EditionCode "RTR"

pattern E_GTC :: EditionCode
pattern E_GTC = EditionCode "GTC"

pattern E_DGM :: EditionCode
pattern E_DGM = EditionCode "DGM"

pattern E_THS :: EditionCode
pattern E_THS = EditionCode "THS"

pattern E_BNG :: EditionCode
pattern E_BNG = EditionCode "BNG"

pattern E_JOU :: EditionCode
pattern E_JOU = EditionCode "JOU"

pattern E_KTK :: EditionCode
pattern E_KTK = EditionCode "KTK"

pattern E_FRF :: EditionCode
pattern E_FRF = EditionCode "FRF"

pattern E_DTK :: EditionCode
pattern E_DTK = EditionCode "DTK"

pattern E_BFZ :: EditionCode
pattern E_BFZ = EditionCode "BFZ"

pattern E_OGW :: EditionCode
pattern E_OGW = EditionCode "OGW"

pattern E_SOI :: EditionCode
pattern E_SOI = EditionCode "SOI"

pattern E_EMN :: EditionCode
pattern E_EMN = EditionCode "EMN"

pattern E_KLD :: EditionCode
pattern E_KLD = EditionCode "KLD"

pattern E_AER :: EditionCode
pattern E_AER = EditionCode "AER"

pattern E_AKH :: EditionCode
pattern E_AKH = EditionCode "AKH"

pattern E_HOU :: EditionCode
pattern E_HOU = EditionCode "HOU"

pattern E_XLN :: EditionCode
pattern E_XLN = EditionCode "XLN"

pattern E_RIX :: EditionCode
pattern E_RIX = EditionCode "RIX"

pattern E_A25 :: EditionCode
pattern E_A25 = EditionCode "A25"

pattern E_DOM :: EditionCode
pattern E_DOM = EditionCode "DOM"

pattern E_BBD :: EditionCode
pattern E_BBD = EditionCode "BBD"

pattern E_M19 :: EditionCode
pattern E_M19 = EditionCode "M19"

pattern E_C18 :: EditionCode
pattern E_C18 = EditionCode "C18"

pattern E_GRN :: EditionCode
pattern E_GRN = EditionCode "GRN"

pattern E_RNA :: EditionCode
pattern E_RNA = EditionCode "RNA"

pattern E_WAR :: EditionCode
pattern E_WAR = EditionCode "WAR"

pattern E_M20 :: EditionCode
pattern E_M20 = EditionCode "M20"

pattern E_C19 :: EditionCode
pattern E_C19 = EditionCode "C19"

{-
pattern E_ :: EditionCode
pattern E_ = EditionCode ""
-}

--------------------------------------------------

pattern CoreEdition :: EditionType
pattern CoreEdition = EditionType "core"

pattern ExpansionEdition :: EditionType
pattern ExpansionEdition = EditionType "expansion"

pattern ReprintEdition :: EditionType
pattern ReprintEdition = EditionType "reprint"

pattern BoxEdition :: EditionType
pattern BoxEdition = EditionType "box"

pattern UnEdition :: EditionType
pattern UnEdition = EditionType "un"

pattern VaultEdition :: EditionType
pattern VaultEdition = EditionType "from the vault"

pattern PremiumEdition :: EditionType
pattern PremiumEdition = EditionType "premium deck"

pattern DuelEdition :: EditionType
pattern DuelEdition = EditionType "duel deck"

pattern StarterEdition :: EditionType
pattern StarterEdition = EditionType "starter"

pattern CommanderEdition :: EditionType
pattern CommanderEdition = EditionType "commander"

pattern PlanechaseEdition :: EditionType
pattern PlanechaseEdition = EditionType "planechase"

pattern ArchenemyEdition :: EditionType
pattern ArchenemyEdition = EditionType "archenemy"

pattern PromoEdition :: EditionType
pattern PromoEdition = EditionType "promo"

pattern VanguardEdition :: EditionType
pattern VanguardEdition = EditionType "vanguard"

pattern MastersEdition :: EditionType
pattern MastersEdition = EditionType "masters"

pattern ConspiracyEdition :: EditionType
pattern ConspiracyEdition = EditionType "conspiracy"

pattern MasterpieceEdition :: EditionType
pattern MasterpieceEdition = EditionType "masterpiece"

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

knownEditionCodes :: [EditionCode]
knownEditionCodes =

  [ E_AL
  , E_BE
  , E_UN
  , E_RV
  , E_SUMMER
  , E_E4
  , E_E5
  , E_E6
  , E_E7
  , E_E8
  , E_E9
  , E_E10
  , E_M10
  , E_M11
  , E_M12
  , E_M13
  , E_M14
  , E_M15
  , E_ORI
  , E_AN
  , E_AQ
  , E_LG
  , E_DK
  , E_FE
  , E_HL
  , E_MR
  , E_VI
  , E_WL
  , E_TP
  , E_SH
  , E_EX
  , E_US
  , E_UL
  , E_UD
  , E_MM
  , E_NE
  , E_PR
  , E_IN
  , E_PS
  , E_AP
  , E_OD
  , E_TR
  , E_JU
  , E_ON
  , E_LE
  , E_SC
  , E_MI
  , E_DS
  , E_DN5
  , E_CHK
  , E_BOK
  , E_SOK
  , E_RAV
  , E_GP
  , E_DI
  , E_IA
  , E_AI
  , E_CS
  , E_TSTS
  , E_TS
  , E_PC
  , E_FUT
  , E_LW
  , E_MT
  , E_SHM
  , E_EVE
  , E_ALA
  , E_CFX
  , E_ARB
  , E_ZEN
  , E_WWK
  , E_ROE
  , E_SOM
  , E_MBS
  , E_NPH
  , E_ISD
  , E_DKA
  , E_AVR
  , E_RTR
  , E_GTC
  , E_DGM
  , E_THS
  , E_BNG
  , E_JOU
  , E_KTK
  , E_FRF
  , E_DTK
  , E_BFZ
  , E_OGW
  , E_SOI
  , E_EMN
  , E_KLD
  , E_AER
  , E_AKH
  , E_HOU
  , E_XLN
  , E_RIX
  , E_A25
  , E_DOM
  , E_BBD
  , E_M19
  , E_C18
  , E_GRN
  , E_RNA
  , E_WAR
  , E_M20
  , E_C19

  ]

--------------------------------------------------

knownEditionTypes :: [EditionType]
knownEditionTypes =

  [ CoreEdition
  , ExpansionEdition
  , ReprintEdition
  , BoxEdition
  , UnEdition
  , VaultEdition
  , PremiumEdition
  , DuelEdition
  , StarterEdition
  , CommanderEdition
  , PlanechaseEdition
  , ArchenemyEdition
  , PromoEdition
  , VanguardEdition
  , MastersEdition
  , ConspiracyEdition
  , MasterpieceEdition
  ]

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------

makePrisms ''EditionName
makePrisms ''EditionCode
makePrisms ''EditionType

makeLenses ''QualifiedEdition
makeLenses ''EditionInfo

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-

-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------