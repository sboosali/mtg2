--------------------------------------------------

{- | Types for /Magic: The Gathering/.

-}

module MTG.Types

  ( module MTG.Types

  -- * Enum types.

  -- * Number types.

  , module MTG.Number.CMC

  -- * List-of-String types.

  , module MTG.Text.List.Colors
  , module MTG.Text.List.ManaCost

  -- * String types.

  , module MTG.Text.Artist

  , module MTG.Text.Block

  , module MTG.Text.Border

  , module MTG.Text.Cardtype

  , module MTG.Text.Color

  , module MTG.Text.Edition

  , module MTG.Text.Format

  , module MTG.Text.Frame

  , module MTG.Text.Keyword

  , module MTG.Text.Language

  , module MTG.Text.Layout

  , module MTG.Text.Legality

  , module MTG.Text.ManaSymbol

  , module MTG.Text.Name

  , module MTG.Text.Rarity

  , module MTG.Text.Subtype

  , module MTG.Text.Supertype

  , module MTG.Text.Symbol

  , module MTG.Text.Watermark

  -- * Classes.

  , module MTG.Classes.Parse
  , module MTG.Classes.Print

  ) where

--------------------------------------------------
-- Exports ---------------------------------------
--------------------------------------------------

import MTG.Number.CMC

--------------------------------------------------

import MTG.Text.List.Colors
import MTG.Text.List.ManaCost

import MTG.Text.Artist
import MTG.Text.Block
import MTG.Text.Border
import MTG.Text.Cardtype
import MTG.Text.Color
import MTG.Text.Edition
import MTG.Text.Format
import MTG.Text.Frame
import MTG.Text.Keyword
import MTG.Text.Language
import MTG.Text.Layout
import MTG.Text.Legality
import MTG.Text.ManaSymbol
import MTG.Text.Name
import MTG.Text.Rarity
import MTG.Text.Subtype
import MTG.Text.Supertype
import MTG.Text.Symbol
import MTG.Text.Watermark

--------------------------------------------------

import MTG.Classes.Parse
import MTG.Classes.Print

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------