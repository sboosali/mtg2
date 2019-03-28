# `MTG`

## Cards



## Queries on Cards

### Examples

idealized example queries (syntax & semantics)...

#### 

```
SUBTYPE in NAME
```

matches any cards such that: for any subtype (no subtype means no match), check if any *word* in that card's name has that subtype, modulo declension.

e.g. `Llanowar Elves` (should match):

`Llanowar Elves` has subtypes `{Elf, Druid}`. `Elves` is the plural of `Elf`. `Elves` is a word in the name. `Elf` is a subtype in the subtype.

NOTES:

- a card's `SUBTYPES` is a set (i.e. unordered, possibly-empty) of strings.

alternate (more verbose, but more realistic) queries:

```
name = stemmed NAME
subtype ∈ SUBTYPES
subtype ~ name
```

```
name    ∈ (map stem (words NAME))
subtype ∈ SUBTYPES
subtype ≍ name
```

where `stem` maps a word to its stem. e.g. `(stem Elf)`, `(stem Elves)`, `(stem Elvish)` all reduce to the stem `Elf—`.

```

```

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

#### 

```

```

