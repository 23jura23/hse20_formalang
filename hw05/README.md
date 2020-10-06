# Parser generator based Prolog Parser

A studying parser of a simplified version of Prolog. Implemented using Alex (Lexer) and Happy (Parser generator)

The parser's output is a syntax tree. It has a compact view for the readability. For example, the nested non-terminals `Disj (Conj (Atom (Id "foo")))` are shown as a simple "foo".

## Prerequisites
* GHC
* [Alex](https://github.com/simonmar/alex)
* [Happy](https://github.com/simonmar/happy)

## Build
```
alex Lex.x
happy Parse.y
ghc main.hs
./main
```

