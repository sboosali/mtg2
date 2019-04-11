
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

import qualified "prettyprinter" Data.Text.Prettyprint.Doc ( Doc, Pretty(..) )
import           "prettyprinter" Data.Text.Prettyprint.Doc as PP

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Map as Map

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

type MTGDoc = Doc Annotations

--------------------------------------------------

{-| Annotation for pretty-printing @mtg-types@.

Set of 'Annotation'(s).

-}

newtype Annotations = Annotations

  [Annotation]

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

--------------------------------------------------

instance IsList Annotations where
  type Item Annotations = Annotation
  fromList = coerce
  toList   = coerce

--------------------------------------------------
--------------------------------------------------

{-| 

-}

data Annotation

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

annOracleText :: Annotations
annOracleText = Annotations [ AnnOracleText ]

annReminderText :: Annotations
annReminderText = Annotations [ AnnReminderText ]

annFlavorText :: Annotations
annFlavorText = Annotations [ AnnFlavorText ]

--------------------------------------------------

annWhite :: Annotations
annWhite = Annotations [ AnnWhite ]

annBlue :: Annotations
annBlue = Annotations [ AnnBlue ]

annBlack :: Annotations
annBlack = Annotations [ AnnBlack ]

annRed :: Annotations
annRed = Annotations [ AnnRed ]

annGreen :: Annotations
annGreen = Annotations [ AnnGreen ]

annColorless :: Annotations
annColorless = Annotations [ AnnColorless ]

--------------------------------------------------

annKeyword :: Annotations
annKeyword = Annotations [ AnnKeyword ]

annPerson :: Annotations
annPerson = Annotations [ AnnPerson ]

annNamesake :: Annotations
annNamesake = Annotations [ AnnNamesake ]

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| Pretty-Print an @Enum@, given an /association list/.

Fails via @Nothing@.

-}

ppAssoc
  :: forall a i.
     Assoc a
  -> (a -> Maybe (Doc i))

ppAssoc kvs = \x ->

  (Map.lookup v vks)

  where

  vks :: Map a (Doc i)
  vks = go kvs

  go = fmap (bimap PP.pretty id)
     > fmap swp
     > Map.fromList

{-# INLINEABLE ppAssoc #-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------