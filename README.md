# AddParens
This package provides an executable that parses an
expression from stdin and then renders that expression
back to stdout after adding explicit parentheses and
layout to the expression. It is intended to be used as
a learning tool to help understand Haskell syntax.

## Build Instructions

### Using `cabal new-build`
```
$ cabal update         # ensure package lists are up-to-date
$ cabal new-run        # build and run the executable
```

### Using `stack`
```
$ stack update         # ensure package lists are up-to-date
$ stack init           # generate a workspace configuration
$ stack setup          # ensure GHC is installed
$ stack build          # build the executable
$ stack exec AddParens # run the executable
```

## Example
```
$ cabal new-run
case a || b || c of
  True  -> f x y
  False -> 4 + 5 * 6  
```
Note: Indicate end of standard input with `Control-D`

*Output*
```
case (a || (b || c)) of { True -> ((f x) y); False -> (4 + (5 * 6))}
```
