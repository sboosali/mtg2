-- -*- sql-product: postgres; -*-

--------------------------------------------------

CREATE TABLE sets

( id   INT  NOT NULL PRIMARY KEY
, name TEXT NOT NULL

);

--------------------------------------------------

-- CREATE TABLE `sets` (
-- baseSetSize INTEGER,
-- block TEXT,
-- boosterV3 TEXT,
-- code TEXT,
-- codeV3 TEXT,
-- isOnlineOnly INTEGER NOT NULL DEFAULT 0,
-- meta TEXT,
-- mtgoCode TEXT,
-- name TEXT,
-- releaseDate TEXT,
-- totalSetSize INTEGER,
-- type TEXT,
-- tcgplayerGroupId INTEGER,
-- isFoilOnly INTEGER NOT NULL DEFAULT 0
-- )

--------------------------------------------------