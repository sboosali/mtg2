# `mtg-json`

## Examples

``` sh
$ mtg-json TODO

```

## CLI

Subcommands (for the `mtg-json` command):

* `fetch` — download, then decompress and/or dearchive.
* `parse` — parse into JSON (`CardObject`). also `fetch`es.
* `check` — parse into Haskell (`Card`). also `parse`s.
* `merge` — merge `Set`s *and\/or* `Card`s *and\/or* `Format`s into one `Format` (recursively). also `check`s.

See:

``` sh
$ mtg-json --help

TODO
```

## Resources

Resources (files, environment variables, websites, port numbers, etc) which this program depends on or makes use of.

Files:

* `~/.config/mtg-json/mtg-json.ini`
* `~/.local/share/mtg-json/`
* `~/.cache/mtg-json/*.{json,tar,gz,zip,tar.gz}`

Environment Variables

* `$https_proxy`
* `$http_proxy`
* `$all_proxy`
* `$XDG_CONFIG_HOME` — Defaults to `~/.config/` on *Linux* and to `%UserProfile%\AppData\Roaming\` on *Windows*.
* `$XDG_DATA_HOME` — Defaults to `~/.local/share/` on *Linux* and to `%UserProfile%\AppData\Roaming\` on *Windows*.
* `$XDG_CACHE_HOME` — Defaults to `~/.cache/` on *Linux* and to `%UserProfile%\AppData\Local\` on *Windows*.

Websites:

* <https://mtgjson.com/> — *MTG JSON*.
* <https://scryfall.com/> — *Scryfall*.

Ports:

* `443` — for `HTTPS` connections.
* `80` — for `HTTP` connections. 

(The lists above are not necessarily exhaustive.)

## Data

`./data/json/*.json`:

* `Vintage.json`
* `Keywords.json` — all MTG Keywords. Grouped by: Ability Words, Keyword Actions, etc.
* `CardTypes.json` — all MTG Card Types. Subtypes are grouped by Supertypes.

Installation was:

* `wget <https://mtgjson.com/json/AllSets.json.zip>` — about `40 MB` when compressed, about `400 MB` when uncompressed.
* `wget <https://mtgjson.com/json/Keywords.json>` 
* `wget <https://mtgjson.com/json/CardTypes.json>` 

## 