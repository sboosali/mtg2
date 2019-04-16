# Implementation Notes


## `base` package

### `System.IO` module

```
hPutStr :: Handle -> String -> IO ()
```

### `Data.Char` module

```
λ> (\(character, category) -> Prelude.putStr [character] >> Prelude.putStr " " >> Prelude.print category) `Prelude.traverse` ((\c -> (c, Char.generalCategory c)) <$> [' '..'~'])

  Space
! OtherPunctuation
" OtherPunctuation
# OtherPunctuation
$ CurrencySymbol
% OtherPunctuation
& OtherPunctuation
' OtherPunctuation
( OpenPunctuation
) ClosePunctuation
* OtherPunctuation
+ MathSymbol
, OtherPunctuation
- DashPunctuation
. OtherPunctuation
/ OtherPunctuation
0 DecimalNumber
1 DecimalNumber
2 DecimalNumber
3 DecimalNumber
4 DecimalNumber
5 DecimalNumber
6 DecimalNumber
7 DecimalNumber
8 DecimalNumber
9 DecimalNumber
: OtherPunctuation
; OtherPunctuation
< MathSymbol
= MathSymbol
> MathSymbol
? OtherPunctuation
@ OtherPunctuation
A UppercaseLetter
B UppercaseLetter
C UppercaseLetter
D UppercaseLetter
E UppercaseLetter
F UppercaseLetter
G UppercaseLetter
H UppercaseLetter
I UppercaseLetter
J UppercaseLetter
K UppercaseLetter
L UppercaseLetter
M UppercaseLetter
N UppercaseLetter
O UppercaseLetter
P UppercaseLetter
Q UppercaseLetter
R UppercaseLetter
S UppercaseLetter
T UppercaseLetter
U UppercaseLetter
V UppercaseLetter
W UppercaseLetter
X UppercaseLetter
Y UppercaseLetter
Z UppercaseLetter
[ OpenPunctuation
\ OtherPunctuation
] ClosePunctuation
^ ModifierSymbol
_ ConnectorPunctuation
` ModifierSymbol
a LowercaseLetter
b LowercaseLetter
c LowercaseLetter
d LowercaseLetter
e LowercaseLetter
f LowercaseLetter
g LowercaseLetter
h LowercaseLetter
i LowercaseLetter
j LowercaseLetter
k LowercaseLetter
l LowercaseLetter
m LowercaseLetter
n LowercaseLetter
o LowercaseLetter
p LowercaseLetter
q LowercaseLetter
r LowercaseLetter
s LowercaseLetter
t LowercaseLetter
u LowercaseLetter
v LowercaseLetter
w LowercaseLetter
x LowercaseLetter
y LowercaseLetter
z LowercaseLetter
{ OpenPunctuation
| MathSymbol
} ClosePunctuation
~ MathSymbol

```

<!-- (\(character, category) -> Prelude.putStr (show character) >> Prelude.putStr " " >> Prelude.print category) `Data.Foldable.traverse_` ((\c -> (c, Char.generalCategory c)) <$> [' '..'~']) -->

```
λ> (\(character, category) -> Prelude.putStr (show character) >> Prelude.putStr " " >> Prelude.print category) `Data.Foldable.traverse_` ((\c -> (c, Char.generalCategory c)) <$> (filter Char.isPunctuation (fmap Char.chr [32..128])))

'!' OtherPunctuation
'"' OtherPunctuation
'#' OtherPunctuation
'%' OtherPunctuation
'&' OtherPunctuation
'\'' OtherPunctuation
'(' OpenPunctuation
')' ClosePunctuation
'*' OtherPunctuation
',' OtherPunctuation
'-' DashPunctuation
'.' OtherPunctuation
'/' OtherPunctuation
':' OtherPunctuation
';' OtherPunctuation
'?' OtherPunctuation
'@' OtherPunctuation
'[' OpenPunctuation
'\\' OtherPunctuation
']' ClosePunctuation
'_' ConnectorPunctuation
'{' OpenPunctuation
'}' ClosePunctuation
```


## `ansi-wl-pprint` package

### `Text.PrettyPrint.ANSI.Leijen` module
 
#### Consumers

> This terminal formatting functionality is, as far as possible, portable across platforms with their varying terminals. However, note that to display ANSI colors and formatting will only be displayed on Windows consoles if the Doc value is output using the `putDoc` function or one of its friends. Rendering the Doc to a String and then outputing that will only work on Unix-style operating systems.

* `show   :: Doc -> String`
* `putDoc :: Doc -> IO ()`

#### Producers

Forecolor combinators:

* `black :: Doc -> Doc`
* `red :: Doc -> Doc`
* `green :: Doc -> Doc`
* `yellow :: Doc -> Doc`
* `blue :: Doc -> Doc`
* `magenta :: Doc -> Doc`
* `cyan :: Doc -> Doc`
* `white :: Doc -> Doc`
* `dullblack :: Doc -> Doc`
* `dullred :: Doc -> Doc`
* `dullgreen :: Doc -> Doc`
* `dullyellow :: Doc -> Doc`
* `dullblue :: Doc -> Doc`
* `dullmagenta :: Doc -> Doc`
* `dullcyan :: Doc -> Doc`
* `dullwhite :: Doc -> Doc`

Backcolor combinators:

* `onblack :: Doc -> Doc`
* `onred :: Doc -> Doc`
* `ongreen :: Doc -> Doc`
* `onyellow :: Doc -> Doc`
* `onblue :: Doc -> Doc`
* `onmagenta :: Doc -> Doc`
* `oncyan :: Doc -> Doc`
* `onwhite :: Doc -> Doc`
* `ondullblack :: Doc -> Doc`
* `ondullred :: Doc -> Doc`
* `ondullgreen :: Doc -> Doc`
* `ondullyellow :: Doc -> Doc`
* `ondullblue :: Doc -> Doc`
* `ondullmagenta :: Doc -> Doc`
* `ondullcyan :: Doc -> Doc`
* `ondullwhite :: Doc -> Doc`

Emboldening combinators:

* `bold :: Doc -> Doc`
* `debold :: Doc -> Doc`

Underlining combinators:

* `underline :: Doc -> Doc`
* `deunderline :: Doc -> Doc`

Formatting elimination combinators:

* `plain :: Doc -> Doc`

## 