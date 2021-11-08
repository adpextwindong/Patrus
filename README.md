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

To make testing against jlox simple I created a [fork of the Crafting Interpreters repo](https://github.com/adpextwindong/craftinginterpreters) that adds a [--lex flag](https://github.com/adpextwindong/craftinginterpreters/commit/77caee73f543396ffb3699ebe6f2ed935af03a69) to jlox. It's added a git submodule. To build it you'll need to install Dart and Java and `make get; make jlox`.

## TODOS

First pass on "A Tree-walk interpreter" in Literal Haskell Style w/ Alex/Happy to see the presentation flow. See [Patrus.lhs](Patrus.lhs) for notes as I go along.

Put all the code into their respective files after first pass.

Switch to Megaparsec eventually. Look at [kleidukos/Intrigue](https://github.com/kleidukos/Intrigue)