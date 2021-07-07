# Saguaro

![header](https://mainbucketbenandrew.s3.eu-west-2.amazonaws.com/images/Saguaro.png)

## Compilation and Usage

```
dune build
./_build/default/source/saguaro.exe FILE
```
`FILE` contains the propositional formula you wish to solve.

## Syntax
```
~   negation
&   and
|   or
=>  implication
<=> iff
```
Precedence decreases downwards. Brackets can be used for explicit precedence.
Variables must be a string of alphabetical ASCII characters.

### Example formulae
- `A <=> ~A`
- `A & C => A | B`
- `A & B & C & D | E`
