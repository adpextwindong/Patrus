# Patrus

![Patrus](res/patrus2.jpg)

A punt on Colin's favorite book ["Crafting Interpreters"](https://craftinginterpreters.com/contents.html) but in Haskell

## Notes

[Syntax and Lexical Grammar Appendix](https://craftinginterpreters.com/appendix-i.html)

[Language Features](LanguageFeatures.md)

Notes from [Chapter 3 on The Lox Language](LoxLanguageNotes.md) 

## Deps

Alex & Happy for lexing/parsing.

```
$ cabal install alex happy
```

## TODOS

First pass on "A Tree-walk interpreter" in Literal Haskell Style w/ Alex/Happy to see the presentation flow. See [Patrus.lhs](Patrus.lhs) for notes as I go along.

Git submodule a fork of just [the Lox tests](https://github.com/munificent/craftinginterpreters/tree/master/test).

Put all the code into their respective files after first pass.

Switch to Megaparsec eventually. Look at [kleidukos/Intrigue](https://github.com/kleidukos/Intrigue)