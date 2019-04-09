# MTG SQL

## Links

* <http://www.postgresqltutorial.com>
* <https://mtgjson.com/files/all-cards/>
* <https://www.johbo.com/2017/on-demand-postgresql-for-your-development-environment.html>
* <https://emacsredux.com/blog/2013/06/13/using-emacs-as-a-database-client/>

## Installation

``` sh
nix-shell -p postgresql

export PGDATA="${XDG_DATA_HOME:-$HOME/.local/share}/postgres"

initdb

pg_ctl start

createdb myproject
```

## Development

Run `psql`

`M-x sql-postgres`

Declare the `.sql` files as `postgres` via `sql-product` (by `add-file-local-variable-prop-line`):

```
-- -*- sql-product: postgres; -*-
```

## Schema

`mtgjson` → `mtgsql`.

Q: normalized or denormalized?

A: 

### Normalized Colors?

Should a card's colors be a single Array Column, or multiple Boolean Columns? i.e.:

```
Name | Color
---- | ----

```

or:

```
Name | White | Blue | Colorless
---- | ---- | --- | ------ | ----

```

## PostgreSQL Types

* `text` — variable length, unlimited length.

* `integer` — 4 bytes, range from `-2147483648` to `+2147483647`.

* `date` — is a date (no time of day), storage is 4 bytes, ranges from 4713 BC to 5874897 AD, resolution is 1 day.

* `boolean` — storage is 1 byte, syntax is `BOOLEAN`.

* `enum` — a (static) ordered set of values, syntax is `CREATE TYPE ... AS ENUM ( ... );`.

e.g. a color `enum`:

```sql
CREATE TYPE color AS ENUM ( 'white', 'blue', 'black', 'red', 'green' );
```

Q: an Enum for all known types (/ subtypes / supertypes)?

A: yes.

Q: an Enum for all known card names?

A: yes, but each name must be about 60 letters long:

>Enum labels must be shorter than `NAMEDATALEN`-bytes long (by default, 63-bytes, a.k.a. 63 ASCII Characters).

## PostgreSQL

### `PostgreSQL `COPY` 

>Export data from table to CSV using COPY statement.

## Related

* <https://github.com/mtgjson/mtgsqlive>
* <https://github.com/Arevor/mtg-sql>

## 