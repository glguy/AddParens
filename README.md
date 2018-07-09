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

## Usage

This command expects to parse a Haskell expression from stdin and then
to write the transformed expression to stdout.

If `type` or `module` are specified as command-line arguments, then
the tool will expect to parse a type or a full module respectively
and will apply the same transformations.

Note: Most terminals use `Control-D` to indicate end of standard input.

## Examples

### Processing an expression

```
$ cabal new-run
case a || b || c of
  True  -> f x y
  False -> 4 + 5 * 6  
```

*Output*
```
case (a || (b || c)) of { True -> ((f x) y); False -> (4 + (5 * 6))}
```

### Processing a module

```
$ cabal new-run AddParens module
data T = U (a -> b -> c)
```

*Output*
```
data T = U (a -> (b -> c))
```
