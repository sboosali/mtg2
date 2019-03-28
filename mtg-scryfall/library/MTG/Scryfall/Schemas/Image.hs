
--------------------------------------------------
--------------------------------------------------

{-|

e.g. an (unparsed) 'ImageObject'

@
{
  "small"       : "https://img.scryfall.com/cards/small/en/m19/314.jpg?1528742053",
  "normal"      : "https://img.scryfall.com/cards/normal/en/m19/314.jpg?1528742053",
  "large"       : "https://img.scryfall.com/cards/large/en/m19/314.jpg?1528742053",
  "png"         : "https://img.scryfall.com/cards/png/en/m19/314.png?1528742053",
  "art_crop"    : "https://img.scryfall.com/cards/art_crop/en/m19/314.jpg?1528742053",
  "border_crop" : "https://img.scryfall.com/cards/border_crop/en/m19/314.jpg?1528742053"
}
@

The image formats are:

IMAGE	SIZE	FORMAT	DESCRIPTION	EXAMPLE
png

745 × 1040

PNG

A transparent, rounded full card PNG. This is the best image to use for videos or other high-quality content.

Example Image

border_crop

480 × 680

JPG

A full card image with the rounded corners and the majority of the border cropped off. Designed for dated contexts where rounded images can’t be used.

Example Image

art_crop

Varies

JPG

A rectangular crop of the card’s art only. Not guaranteed to be perfect for cards with outlier designs or strange frame arrangements

Example Image

large

672 × 936

JPG

A large full card image

Example Image

normal

488 × 680

JPG

A medium-sized full card image

Example Image

small

146 × 204

JPG

A small full card image. Designed for use as thumbnail or list icon.

Example Image

Please note that newly released cards or particularly esoteric promos may not yet have high-resolution imagery.


-}

module MTG.Scryfall.Schemas.Image

  ( ImageObject(..)

  ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--------------------------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------

import Prelude.MTG.Scryfall

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

data ImageObject = ImageObject

  { png         :: Url -- ^ e.g. @"https://img.scryfall.com/cards/png/en/m19/314.png?1528742053"@
  , small       :: Url -- ^ e.g. @"https://img.scryfall.com/cards/small/en/m19/314.jpg?1528742053"@
  , normal      :: Url -- ^ e.g. @"https://img.scryfall.com/cards/normal/en/m19/314.jpg?1528742053"@
  , large       :: Url -- ^ e.g. @"https://img.scryfall.com/cards/large/en/m19/314.jpg?1528742053"@
  , art_crop    :: Url -- ^ e.g. @"https://img.scryfall.com/cards/art_crop/en/m19/314.jpg?1528742053"@
  , border_crop :: Url -- ^ e.g. @"https://img.scryfall.com/cards/border_crop/en/m19/314.jpg?1528742053"@
  }

--------------------------------------------------
--------------------------------------------------

type Url = String

--------------------------------------------------
--------------------------------------------------

-- contentTypeOfImageObject :: ImageObject
-- contentTypeOfImageObject = ImageObject
--   { png         = "png"
--   , small       = "jpg"
--   , normal      = "jpg"
--   , large       = "jpg"
--   , art_crop    = "jpg"
--   , border_crop = "jpg"
--   }

--------------------------------------------------
--------------------------------------------------



--------------------------------------------------
--------------------------------------------------
