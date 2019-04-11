
--------------------------------------------------
--------------------------------------------------

{-| 

== Implementation

The 'CharParsing' @class@ from the @parser@ package has @instance@s for:

* @attoparsec@ — used by @mtg-json@ (via the @aeson@ package).
* @trifecta@ — used for pretty-printed parse-errors (e.g. @ANSI@ color codes, “caret diagnostics”).
* @base@'s "Text.Read" — useful for simple and\/or standalone examples (e.g. in @doctest@s).

-}

module MTG.Classes.Print

  ( module MTG.Classes.Print

  , Doc
  , Pretty(pretty)
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Classes.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc as PP
import           "prettyprinter" Data.Text.Prettyprint.Doc ( Doc, Pretty(..) )

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type MTGDocument = Doc MTGAnnotations

--------------------------------------------------

{-| Annotation for pretty-printing @mtg-types@.

Set of 'MTGAnnotation'(s).

-}

newtype MTGAnnotations = MTGAnnotations

  [MTGAnnotation]

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsList MTGAnnotations where
  type Item MTGAnnotations = MTGAnnotation
  fromList = coerce
  toList   = coerce

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data MTGAnnotation

  = AnnOracleText
  | AnnReminderText
  | AnnFlavorText

  | AnnWhite
  | AnnBlue
  | AnnBlack
  | AnnRed
  | AnnGreen
  | AnnColorless

  | AnnKeyword
  | AnnPerson
  | AnnNamesake

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)

  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

annOracleText :: MTGAnnotations
annOracleText = MTGAnnotations [ AnnOracleText ]

annReminderText :: MTGAnnotations
annReminderText = MTGAnnotations [ AnnReminderText ]

annFlavorText :: MTGAnnotations
annFlavorText = MTGAnnotations [ AnnFlavorText ]

--------------------------------------------------

annWhite :: MTGAnnotations
annWhite = MTGAnnotations [ AnnWhite ]

annBlue :: MTGAnnotations
annBlue = MTGAnnotations [ AnnBlue ]

annBlack :: MTGAnnotations
annBlack = MTGAnnotations [ AnnBlack ]

annRed :: MTGAnnotations
annRed = MTGAnnotations [ AnnRed ]

annGreen :: MTGAnnotations
annGreen = MTGAnnotations [ AnnGreen ]

annColorless :: MTGAnnotations
annColorless = MTGAnnotations [ AnnColorless ]

--------------------------------------------------

annKeyword :: MTGAnnotations
annKeyword = MTGAnnotations [ AnnKeyword ]

annPerson :: MTGAnnotations
annPerson = MTGAnnotations [ AnnPerson ]

annNamesake :: MTGAnnotations
annNamesake = MTGAnnotations [ AnnNamesake ]

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | 

-}

renderString_MTGDocument :: MTGDocument -> String
renderString_MTGDocument = runPrinter

--------------------------------------------------

{- | 

-}

-- renderANSI_MTGDocument :: MTGDocument -> String
-- renderANSI_MTGDocument = _



--------------------------------------------------

{-| Pretty-Print an @Enum@, given an /association list/.

Fails via @Nothing@.

-}

ppAssoc
  :: forall a i.
     ( Ord a
     )
  => Assoc a
  -> (a -> Maybe (Doc i))

ppAssoc kvs = \v ->

  (Map.lookup v vks)

  where

  vks :: Map a (Doc i)
  vks = go kvs

  go = fmap (bimap PP.pretty id)
     > fmap swap
     > Map.fromList

{-# INLINEABLE ppAssoc #-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------