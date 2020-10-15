# Parser Combinator based Prolog Parser

A studying parser of a simplified version of Prolog. Implemented using Parsec (Parser Combinator) in Haskell

The parser's output is a syntax tree. It has a compact view for the readability. For example, the nested non-terminals `Disj (Conj (Atom (Id "foo")))` are shown as a simple `"foo"`.

## Prerequisites
* GHC
* [Parsec](https://hackage.haskell.org/package/parsec)

## Build
```
ghc main.hs
./main
```

