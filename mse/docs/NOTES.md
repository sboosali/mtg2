# MSE

## `.mse` files

a `.mse` file is a `.tar` file.

### MSE Syntax

MSE Strings can embded:

* XML — for styling (or rarely, for semantic markup).
* Curly-Braces — for interpolating MSE Scripts.

e.g. a custom keyword:

```conf
keyword:
	keyword: Research
	match: research <atom-param>number</atom-param>
	reminder: Create {english_number_a(param1)} colorless Experiment artifact token{if param1.value == 1 then "" else "s"}. {if param1.value == 1 then "It has" else "They have"} [1], Sacrifice this artifact: Draw a card.
	rules:
	mode: custom
```

e.g. MSE XML:

```conf
match: research <atom-param>number</atom-param>
```

e.g. MSE (interpolated) Scripts:

```conf
reminder: Create {english_number_a(param1)} colorless Experiment artifact token{if param1.value == 1 then "" else "s"}. {if param1.value == 1 then "It has" else "They have"} [1], Sacrifice this artifact: Draw a card.
```

## MSE XML-Markup

```xml
research <atom-param>number</atom-param>
```

## MSE Scripts

e.g. Application:

```mse
english_number_a(param1)
```

e.g. Conditional:

```mse
if param1.value == 1 then "It has" else "They have"
```

## 

