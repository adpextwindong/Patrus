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

- [ ] Primitives
    - [ ] Booleans
    - [ ] Double-Precision Floats
    - [ ] Integers
    - [ ] Strings
    - [ ] Nil
- [ ] Expressions
    - [ ] Arithmetic
        - [ ] Add
        - [ ] Subtract
        - [ ] Multiply
        - [ ] Divide
        - [ ] Negate
    - [ ] String Concatenation
    - [ ] Comparison
        - [ ] LT
        - [ ] LTE
        - [ ] GT
        - [ ] GTE
    - [ ] Equality
    - [ ] Logical Operators
        - [ ] And
        - [ ] Or
        - [ ] Not
        - [ ] Short Circuiting
    - [ ] Precendence and Grouping
- [ ] Statements
    - [ ] Statement Blocks
    - [ ] Variable Declaration
        - [ ] Nil as default
    - [ ] Variable Assignment
    - [ ] Control Flow
        - [ ] If-else
        - [ ] While
        - [ ] C-style forloop
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
    - [ ] Built-in `print`
    - [ ] Clock Function for benchmarking

## Lexer Notes

- [x] Multi Line String Literals

No escaped characters, quotemarks for example, have to be lexed so this turned out easier than expected.
