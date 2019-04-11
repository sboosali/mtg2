
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
  , Pretty(pretty)
  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Classes.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "prettyprinter" Data.Text.Prettyprint.Doc ( Pretty(..) )
import           "prettyprinter" Data.Text.Prettyprint.Doc as PP

--------------------------------------------------
-- Types -----------------------------------------
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

  | Ann

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{-| Pretty-Print an @Enum@ via an /association list/.

-}

ppAssoc :: Assoc a ->  a
ppAssoc kvs = do

  let pKvs = (pPair <$> kvs)

  (foldr (<|>) empty) pKvs

  where

  pPair (k,v) = v <$ P.text k

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------