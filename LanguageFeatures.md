# Language Features

Having tests for these too would be nice. See [examples\lox](examples\lox).

## Lox Summary

- Imperative
- Dynamic Typing (No implicit conversion)
- Closures
- First Class Functions
- Lexical Scope
- Single inheritance OOP, Class Constructors Inherited

## Feature List

- [x] Primitives
    - [x] Booleans
    - [x] Double-Precision Floats
    - [x] Integers
    - [x] Strings
    - [x] Nil
- [x] Expressions
    - [x] Arithmetic
        - [x] Add
        - [x] Subtract
        - [x] Multiply
        - [x] Divide
        - [x] Negate
    - [x] String Concatenation
    - [x] Comparison
        - [x] LT
        - [x] LTE
        - [x] GT
        - [x] GTE
    - [x] Equality
    - [x] Precendence and Grouping
- [x] Statements
    - [x] Statement Blocks
    - [x] Variable Declaration
        - [x] Nil as default
    - [x] Variable Assignment
    - [x] Lexical Scoping
    - [x] Control Flow
        - [x] If-else
        - [x] While
        - [x] C-style forloop
    - [ ] Function Declaration
        - [ ] Nil as default return value if return ommited.
    - [ ] Class Declaration
    - [ ] Class Instance Creation Statement
- [ ] Functions
    - [ ] Function Call
    - [ ] First Class Functions
- [ ] Closures
- [ ] Classes
    - [ ] Class Methods & Fields
    - [ ] `this` keyword
    - [ ] Class Constructor
    - [ ] Single Inheritance
        - [ ] Superclass Methods
        - [ ] Class Constructor Inheritance aka `super` keyword
- [ ] "Standard Library"
    - [x] Built-in `print`
    - [ ] Clock Function for benchmarking

## Lexer Notes

- [x] Multi Line String Literals

No escaped characters, quotemarks for example, have to be lexed so this turned out easier than expected.
