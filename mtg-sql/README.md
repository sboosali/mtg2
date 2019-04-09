# MTG SQL

## Links

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

## Related

* <https://github.com/mtgjson/mtgsqlive>
* <https://github.com/Arevor/mtg-sql>

## 