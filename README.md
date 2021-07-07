# Saguaro

![header](https://mainbucketbenandrew.s3.eu-west-2.amazonaws.com/images/Saguaro.png)

## Instructions for compilation
Using `opam 2.0.8` and MirageOS `4.0.0`
### Installing `mirage 4.0.0`
```
opam repo add mirage-dev https://github.com/mirage/mirage-dev.git#mirage-4
opam repo add opam-overlays https://github.com/mirage/opam-overlays.git
opam update -yu
opam install mirage.4.0.0
```
### Compilation
```
cd source
mirage configure -t TARGET --no-extra-repo
# Delete '@$(MAKE) -s depext-lockfile' from the 'depend' clause of 'Makefile'
make depend
# Delete the 'ocaml-base-compiler' dependency from 'mirage/<unikernel>.opam.locked'
make -s depext-lockfile
mirage build
```
`TARGET` is either `unix` or `macosx`.

### Running
```
./_build/default/main.exe --formula='<formula>'
```

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