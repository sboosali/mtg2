# Implementation Notes

## `Data.Char`

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



## 