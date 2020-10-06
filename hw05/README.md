# Parser generator based Prolog Parser

A studying parser of a simplified version of Prolog. Implemented using Alex (Lexer) and Happy (Parser generator)

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

