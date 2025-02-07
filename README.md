# Sed clone

A simple clone of the sed command line utility.

It implements, from what I know, all POSIX features with some additions:
- part of the extended regular expressions (without e.g backreferences or character classes) (activated by default)

## building

```bash
dune build
```

## running

```bash
dune exec sed -- <args>
dune exec sed -- -f ./test/flip.sed <<<"hello"
dune exec sed -- -e "s/hello/world/"
dune exec sed -- "s/hello/world/"
dune exec sed -- -f ./test/reverse.sed
```

## testing

```bash
dune runtest
```

## some additional info

- Regexes are implemented (when apllicable) with a DFA.
- With one pass over the string we can determine if there exists a match for the regex (for e.g addresses).
- Additionally when we want to have something more than just detection, we find the first M matches in M+1 passes over the string.
- There is little to no mutable state (only in DFA construction).
