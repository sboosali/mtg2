--------------------------------------------------
-- Extensions ------------------------------------
--------------------------------------------------

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE PatternSynonyms       #-}

--------------------------------------------------

{-| 

== Examples

-}

module MTG.Enum.Color where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import MTG.Types.Prelude

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--import "lens" Control.Lens (makePrisms)

--------------------------------------------------

import qualified "formatting" Formatting as Format
import           "formatting" Formatting ((%))

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "base" Control.Exception as E
import           "base" Control.Exception (PatternMatchFail(..))

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------
-- Zero-or-more Colors...

{-| @0-5@ Colors.

A set of /known/ colors (in /Magic: The Gathering/).

-}

data Colors

  = ZeroColors
  | OneColor    Color
  | TwoColors   Guild
  | ThreeColors ShardOrWedge
  | FourColors  Nephilim
  | FiveColors

  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
-- One Color...

{-| One color.

A /known/ color.

-}

data Color

  = White
  | Blue
  | Black
  | Red
  | Green

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
-- Two (/Guild) Colors...

{- | Two colors.

Like an unordered pair of 'Color's.

Named after the /Ravnica/ guilds.

-}

data Guild

  = Azorius
  | Dimir
  | Rakdos
  | Gruul
  | Selesnya

  | Orzhov
  | Izzet
  | Golgari
  | Boros
  | Simic

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
-- Three (/Shard/Wedge) Colors...

{- | Three colors.

Like an unordered triplet of 'Color'.

Named after the /Khans/ clans. Either a wedge or a shard (a "slice of the color pie").

-}

data ShardOrWedge

  = Bant
  | Esper
  | Grixis
  | Jund
  | Naya

  | Mardu
  | Temur
  | Abzan
  | Jeskai
  | Sultai
         
  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
-- Four (/Nephilim) Colors...

{- | Four colors.

Like an unordered quadruple of 'Color's.

Named after the @Nephilim@.

The /Nephilim/ from /Guildpact/ were the first four-colored cards. /Commander 2016/ introduced a second cycle of four-colored cards.

-}

data Nephilim

  = Artifice
  | Chaos
  | Aggression
  | Altruism
  | Growth

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------
-- Five (/all) Colors...

{- | Five colors.

Like a quintuple of 'Color's.

-}

data WUBRG

  = WUBRG

  deriving stock    (Enum,Bounded,Ix)
  deriving anyclass (GEnum)
  deriving stock    (Show,Read,Eq,Ord)
  deriving stock    (Generic,Lift)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
-- Patterns --------------------------------------
--------------------------------------------------

pattern AzoriusColors :: [Color]
pattern AzoriusColors = [ White, Blue]

pattern DimirColors :: [Color]
pattern DimirColors = [ Blue, Black ]

pattern RakdosColors :: [Color]
pattern RakdosColors = [ Black, Red ]

pattern GruulColors :: [Color]
pattern GruulColors = [ Red, Green ]

pattern SelesnyaColors :: [Color]
pattern SelesnyaColors = [ Green, White ]

pattern OrzhovColors :: [Color]
pattern OrzhovColors = [ White, Black ]

pattern IzzetColors :: [Color]
pattern IzzetColors = [ Blue, Red ]

pattern GolgariColors :: [Color]
pattern GolgariColors = [ Black, Green ]

pattern BorosColors :: [Color]
pattern BorosColors = [ White, Red ]

pattern SimicColors :: [Color]
pattern SimicColors = [ Green, Blue ]

--------------------------------------------------

pattern BantColors :: [Color]
pattern BantColors = [ Green, White, Blue ]

pattern EsperColors :: [Color]
pattern EsperColors = [ White, Blue, Black ]

pattern GrixisColors :: [Color]
pattern GrixisColors = [ Blue, Black, Red ]

pattern JundColors :: [Color]
pattern JundColors = [ Black, Red, Green ]

pattern NayaColors :: [Color]
pattern NayaColors = [ Red, Green, White ]

pattern MarduColors :: [Color]
pattern MarduColors = [ Red, White, Black ]

pattern TemurColors :: [Color]
pattern TemurColors = [ Green, Blue, Red ]

pattern AbzanColors :: [Color]
pattern AbzanColors = [ White, Black, Green ]

pattern JeskaiColors :: [Color]
pattern JeskaiColors = [ Blue, Red, White ]

pattern SultaiColors :: [Color]
pattern SultaiColors = [ Black, Green, Blue ]

--------------------------------------------------

pattern ArtificeColors :: [Color]
pattern ArtificeColors = [ White, Blue, Black, Red ]

pattern ChaosColors :: [Color]
pattern ChaosColors = [ Blue, Black, Red, Green ]

pattern AggressionColors :: [Color]
pattern AggressionColors = [ Black, Red, Green, White ]

pattern AltruismColors :: [Color]
pattern AltruismColors = [ Red, Green, White, Blue ]

pattern GrowthColors :: [Color]
pattern GrowthColors = [ Green, White, Blue, Black ]

--------------------------------------------------
-- Constants -------------------------------------
--------------------------------------------------

-- | @≡ [ 'White', 'Blue', 'Black', 'Red', 'Green' ]@
allColors :: [Color]
allColors = [ White, Blue, Black, Red, Green ]

--------------------------------------------------
-- Functions: Constructors -----------------------
--------------------------------------------------

{- | Refine a 'Color' list into a total 'Color' set.

`toColors`:

* removes duplicates
* sorts canonically
* represents inductively

== Definition

@
≡ 'toColorsM' > either 'E.throw' 'return'
@

== Safety

**NOTE** `toColors` /should always/ be total (i.e. as total as `toColorsM`).

-}

toColors :: [Color] -> Colors
toColors cs = toColorsM_Either & (either E.throw id)
  where

  toColorsM_Either :: Either SomeException Colors
  toColorsM_Either = toColorsM cs

--------------------------------------------------

{- | Like `toColors`.

== Exceptions

May throw 'PatternMatchFail'.

== Safety

**NOTE** `toColorsM` /should never/ throw
(if it does, file an issue; <https://github.com/sboosali/mtg/issues/new>).

-}

toColorsM :: (MonadThrow m) => [Color] -> m Colors
toColorsM cs = cs & (ordNub > sort > go)
  where

  go = \case

      []                                 -> return ZeroColors
      [a]                                -> return $ OneColor a
      [a,b]                              -> (toGuild        a b)     & maybe (throwM e) (return . TwoColors)
      [a,b,c]                            -> (toShardOrWedge a b c)   & maybe (throwM e) (return . ThreeColors)
      [a,b,c,d]                          -> (toNephilim     a b c d) & maybe (throwM e) (return . FourColors)
      [ White, Blue, Black, Red, Green ] -> return FiveColors

      _                                  -> throwM e -- (NOTE: this case should never be reached.)

  e :: PatternMatchFail
  e = PatternMatchFail (runFormat ("``` " % Format.string % " ```: {{{ toColors " % Format.string % " }}} ")
                        (displayName 'toColors)
                        (showWithApplicationPrecedence cs)
                       )

--------------------------------------------------

{- | Construct a `Guild` from two /distinct/ colors.

== Examples

>>> toGuild Blue Green
Simic

== Laws

`toGuild` is **symmetric**.

i.e.:

@∀ x y. toGuild x y ≡ toGuild y x@

e.g.:

>>> (toGuild Blue Green :: Maybe Guild) == (toGuild Green Blue :: Maybe Guild)
True

`toGuild` is **non-reflexive** (?).

i.e.:

@∀ x. ! toGuild x x@

e.g.:

>>> toGuild Blue Blue :: Maybe Guild
Nothing
>>> toGuild Green Green :: Maybe Guild
Nothing

-}

toGuild :: (MonadThrow m) => Color -> Color -> m Guild

toGuild White Blue  = return Azorius  
toGuild Blue  White = return Azorius  

toGuild Blue  Black = return Dimir    
toGuild Black Blue  = return Dimir    

toGuild Black Red   = return Rakdos   
toGuild Red   Black = return Rakdos   

toGuild Red   Green = return Gruul    
toGuild Green Red   = return Gruul    

toGuild Green White = return Selesnya 
toGuild White Green = return Selesnya 

toGuild White Black = return Orzhov
toGuild Black White = return Orzhov

toGuild Black Green = return Golgari  
toGuild Green Black = return Golgari  

toGuild Green Blue  = return Simic    
toGuild Blue  Green = return Simic    

toGuild Blue  Red   = return Izzet    
toGuild Red   Blue  = return Izzet    

toGuild Red   White = return Boros    
toGuild White Red   = return Boros

toGuild x y = throwM e
  where

  e :: PatternMatchFail
  e = PatternMatchFail (runFormat ("``` " % Format.string % " ```: {{{ toGuild " % Format.string % " " % Format.string % " }}} ")
                        (displayName 'toGuild)
                        (showWithApplicationPrecedence x)
                        (showWithApplicationPrecedence y)
                       )

{-# INLINEABLE toGuild #-}

--------------------------------------------------

{- | Construct a `ShardOrWedge` from three /distinct/ colors.

-}

toShardOrWedge :: (MonadThrow m) => Color -> Color -> Color -> m ShardOrWedge

toShardOrWedge Green White Blue  = return Bant
toShardOrWedge White Blue  Green = return Bant
toShardOrWedge Blue  Green White = return Bant
toShardOrWedge Green Blue  White = return Bant
toShardOrWedge White Green Blue  = return Bant
toShardOrWedge Blue  White Green = return Bant

toShardOrWedge White Blue  Black = return Esper
toShardOrWedge Blue  Black White = return Esper
toShardOrWedge Black White Blue  = return Esper
toShardOrWedge White Black Blue  = return Esper
toShardOrWedge Blue  White Black = return Esper
toShardOrWedge Black Blue  White = return Esper

toShardOrWedge Blue  Black Red   = return Grixis
toShardOrWedge Black Red   Blue  = return Grixis
toShardOrWedge Red   Blue  Black = return Grixis
toShardOrWedge Blue  Red   Black = return Grixis
toShardOrWedge Black Blue  Red   = return Grixis
toShardOrWedge Red   Black Blue  = return Grixis

toShardOrWedge Black Red   Green = return Jund
toShardOrWedge Red   Green Black = return Jund
toShardOrWedge Green Black Red   = return Jund
toShardOrWedge Black Green Red   = return Jund
toShardOrWedge Red   Black Green = return Jund
toShardOrWedge Green Red   Black = return Jund

toShardOrWedge Red   Green White = return Naya
toShardOrWedge Green White Red   = return Naya
toShardOrWedge White Red   Green = return Naya
toShardOrWedge Red   White Green = return Naya
toShardOrWedge Green Red   White = return Naya
toShardOrWedge White Green Red   = return Naya

toShardOrWedge Red   White Black = return Mardu
toShardOrWedge White Black Red   = return Mardu
toShardOrWedge Black Red   White = return Mardu
toShardOrWedge Red   Black White = return Mardu
toShardOrWedge White Red   Black = return Mardu
toShardOrWedge Black White Red   = return Mardu

toShardOrWedge Green Blue  Red   = return Temur
toShardOrWedge Blue  Red   Green = return Temur
toShardOrWedge Red   Green Blue  = return Temur
toShardOrWedge Green Red   Blue  = return Temur
toShardOrWedge Blue  Green Red   = return Temur
toShardOrWedge Red   Blue  Green = return Temur

toShardOrWedge White Black Green = return Abzan
toShardOrWedge Black Green White = return Abzan
toShardOrWedge Green White Black = return Abzan
toShardOrWedge White Green Black = return Abzan
toShardOrWedge Black White Green = return Abzan
toShardOrWedge Green Black White = return Abzan

toShardOrWedge Blue  Red   White = return Jeskai
toShardOrWedge Red   White Blue  = return Jeskai
toShardOrWedge White Blue  Red   = return Jeskai
toShardOrWedge Blue  White Red   = return Jeskai
toShardOrWedge Red   Blue  White = return Jeskai
toShardOrWedge White Red   Blue  = return Jeskai

toShardOrWedge Black Green Blue  = return Sultai
toShardOrWedge Green Blue  Black = return Sultai
toShardOrWedge Blue  Black Green = return Sultai
toShardOrWedge Black Blue  Green = return Sultai
toShardOrWedge Green Black Blue  = return Sultai
toShardOrWedge Blue  Green Black = return Sultai

toShardOrWedge x y z = throwM e
  where

  e :: PatternMatchFail
  e = PatternMatchFail (runFormat ("``` " % Format.string % " ```: {{{ toShardOrWedge " % Format.string % " " % Format.string % " " % Format.string % " }}} ")
                        (displayName 'toShardOrWedge)
                        (showWithApplicationPrecedence x)
                        (showWithApplicationPrecedence y)
                        (showWithApplicationPrecedence z)
                       )

{-# INLINEABLE toShardOrWedge #-}

--------------------------------------------------

{- | Construct a `Nephilim` from four /distinct/ colors.

-}

toNephilim :: (MonadThrow m) => Color -> Color -> Color -> Color -> m Nephilim

toNephilim w x y z = case sort [ w, x, y, z ] of

  [ White, Blue,  Black, Red   ] -> return Artifice
  [ Blue,  Black, Red,   Green ] -> return Chaos
  [ White, Black, Red,   Green ] -> return Aggression
  [ White, Blue,  Red,   Green ] -> return Altruism
  [ White, Blue,  Black, Green ] -> return Growth

  _ -> throwM e

  where

  e :: PatternMatchFail
  e = PatternMatchFail (runFormat ("``` " % Format.string % " ```: {{{ toNephilim " % Format.string % " " % Format.string % " " % Format.string % " " % Format.string % " }}} ")
                        (displayName 'toNephilim)
                        (showWithApplicationPrecedence w)
                        (showWithApplicationPrecedence x)
                        (showWithApplicationPrecedence y)
                        (showWithApplicationPrecedence z)
                       )

{-# INLINEABLE toNephilim #-}

--------------------------------------------------
-- Functions: Destructors ------------------------
--------------------------------------------------

fromColors :: Colors -> [Color]
fromColors = \case

  ZeroColors     -> []
  OneColor    c1 -> fromColor        c1
  TwoColors   c2 -> fromGuild        c2
  ThreeColors c3 -> fromShardOrWedge c3
  FourColors  c4 -> fromNephilim     c4
  FiveColors     -> allColors

--------------------------------------------------

fromColor :: Color -> [Color]
fromColor = (: [])

--------------------------------------------------

fromGuild :: Guild -> [Color]
fromGuild = \case

  Azorius  -> AzoriusColors
  Dimir    -> DimirColors
  Rakdos   -> RakdosColors
  Gruul    -> GruulColors
  Selesnya -> SelesnyaColors

  Orzhov   -> OrzhovColors
  Izzet    -> IzzetColors
  Golgari  -> GolgariColors
  Boros    -> BorosColors
  Simic    -> SimicColors

--------------------------------------------------

fromShardOrWedge :: ShardOrWedge -> [Color]
fromShardOrWedge = \case

  Bant   -> BantColors
  Esper  -> EsperColors
  Grixis -> GrixisColors
  Jund   -> JundColors
  Naya   -> NayaColors

  Mardu  -> MarduColors
  Temur  -> TemurColors
  Abzan  -> AbzanColors
  Jeskai -> JeskaiColors
  Sultai -> SultaiColors

--------------------------------------------------

fromNephilim :: Nephilim -> [Color]
fromNephilim = \case

  Artifice   -> ArtificeColors
  Chaos      -> ChaosColors
  Aggression -> AggressionColors
  Altruism   -> AltruismColors
  Growth     -> GrowthColors

--------------------------------------------------
-- Functions -------------------------------------
--------------------------------------------------

{- | Sort canonically (by /Magic: The Gathering/'s conventions).

== Laws

`sortMTGColors` is:

[/idempotent/]

     @(`sortMTGColors` . `sortMTGColors`) ≡ `sortMTGColors`@

== Notes

/NOTE/ @Color@s in @Wedge@s are sorted not as @Shard@s. In particular:

>>> TemurColors
[Green,Blue,Red]
>>> sortMTGColors TemurColors
[Green,Blue,Red]
>>> sort TemurColors
[Blue,Red,Green]

i.e. `sort` (whose comparison relation follows `Ord`) differs from `sortMTGColors`.

/NOTE/ Furthermore, the relation
implied by this sorting (i.e. by `sortMTGColors`) is **not** transitive,
and thus would be invalid as a standalone comparison. In particular:

>>> sortMTGColors OrzhovColors
[White,Black]
>>> sortMTGColors SelesnyaColors
[Green,White]
>>> sortMTGColors GolgariColors
[Black,Green]

i.e.:

* `White` precedes `Black` (when paired)
* `Black` precedes `Green` (when paired)
* but `White` /doesn't/ precede `Green` (when paired), which it would by **transitivity**,
  and which it does via @(>=)@.

(where a “relation” here means “a value of type @"Data.Functor.Contravariant.Comparison" 'Color'@”.)

-}

sortMTGColors :: [Color] -> [Color]
sortMTGColors colorsList = colorsSorted
  
  where

  colorsSorted = colorsSet & maybe colorsList fromColors
  colorsSet    = toColorsM colorsList

--------------------------------------------------
-- Optics ----------------------------------------
--------------------------------------------------


--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------
{-
--------------------------------------------------

Naming:

- Colors
- ColorSet
- Polychrome

Naming:

- ThreeColors
- Trichrome
- ShardOrWedge
- Dega, Ceta, Necra, Raka, Ana

Naming:

- FourColors
- Tetrachrome
- Nephilim

Naming:

- FiveColors
- Pentachrome
- WUBRG

--------------------------------------------------

Explicitly-Bidirectional Pattern-Synonym:

    pattern HeadC x <- x:xs where
      HeadC x = [x]

--------------------------------------------------
-}
--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------