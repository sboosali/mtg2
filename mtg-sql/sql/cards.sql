--------------------------------------------------

CREATE TABLE cards

( id   INT  NOT NULL PRIMARY KEY

, name  TEXT NOT NULL
, names ARRAY TEXT,

artist                TEXT,
borderColor           TEXT,
colorIdentity         TEXT,
colorIndicator        TEXT,
colors                TEXT,
convertedManaCost     INTEGER,
flavorText            TEXT,
frameVersion          TEXT,
layout                TEXT,
loyalty               TEXT,
manaCost              TEXT,
multiverseId          INTEGER,
collectorsNumber      TEXT,
originalText          TEXT,
originalType          TEXT,
printings             TEXT,
power                 TEXT,
rarity                TEXT,
scryfallId            TEXT,
setCode               TEXT,
side                  TEXT,
subtypes              TEXT,
supertypes            TEXT,
text                  TEXT,
toughness             TEXT,
type                  TEXT,
types                 TEXT,
variations            TEXT,
watermark             TEXT

-- uuid                  TEXT(36) PRIMARY KEY
);

--------------------------------------------------

-- artist                TEXT,
-- borderColor           TEXT,
-- colorIdentity         TEXT,
-- colorIndicator        TEXT,
-- colors                TEXT,
-- convertedManaCost     INTEGER,
-- flavorText            TEXT,
-- frameEffect           TEXT,
-- frameVersion          TEXT,
-- hand                  TEXT,
-- hasFoil               INTEGER NOT NULL DEFAULT 0,
-- hasNonFoil            INTEGER NOT NULL DEFAULT 0,
-- isAlternative         INTEGER NOT NULL DEFAULT 0,
-- isFoilOnly            INTEGER NOT NULL DEFAULT 0,
-- isOnlineOnly          INTEGER NOT NULL DEFAULT 0,
-- isOversized           INTEGER NOT NULL DEFAULT 0,
-- isReserved            INTEGER NOT NULL DEFAULT 0,
-- starter               INTEGER NOT NULL DEFAULT 0,
-- isTimeshifted         INTEGER NOT NULL DEFAULT 0,
-- layout                TEXT,
-- life                  TEXT,
-- loyalty               TEXT,
-- manaCost              TEXT,
-- multiverseId          INTEGER,
-- name                  TEXT,
-- names                 TEXT,
-- number                TEXT,
-- originalText          TEXT,
-- originalType          TEXT,
-- printings             TEXT,
-- power                 TEXT,
-- rarity                TEXT,
-- scryfallId            TEXT,
-- setCode               TEXT,
-- side                  TEXT,
-- subtypes              TEXT,
-- supertypes            TEXT,
-- tcgplayerProductId    INTEGER,
-- tcgplayerPurchaseUrl  TEXT,
-- text                  TEXT,
-- toughness             TEXT,
-- type                  TEXT,
-- types                 TEXT,
-- uuid                  TEXT(36) PRIMARY KEY,
-- variations            TEXT,
-- watermark             TEXT

--------------------------------------------------