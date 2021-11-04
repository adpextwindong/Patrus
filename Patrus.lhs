1.3 - The First Interpreter

"We’ll write the simplest, cleanest code we can to correctly implement the semantics of the language."

2.1.1 - Scanning

For now I'll use Alex to write the Lexer.

2.1.2 - Parsing

For now I'll use Happy to generate a parser.

2.1.3 Static Analysis

"The language we’ll build in this book is dynamically typed, so it will do its type checking later, at runtime."

2.2.2 Tree-walk interpreters

"Some programming languages begin executing code right after parsing it to an AST (with maybe a bit of static analysis applied). To run the program, the interpreter traverses the syntax tree one branch and leaf at a time, evaluating each node as it goes."

"A notable exception is early versions of Ruby, which were tree walkers. At 1.9, the canonical implementation of Ruby switched from the original MRI (Matz’s Ruby Interpreter) to Koichi Sasada’s YARV (Yet Another Ruby VM). YARV is a bytecode virtual machine."

The Lox Language

3.1 Hello, Lox

```lox
// Your first Lox program!
print "Hello, world!";
```

3.2 A High-Level Language

"As we’ll learn later, Lox’s approach to scoping hews closely to Scheme."

3.3 Data Types

Lox primitives:

- Booleans
```lox
true;  // Not false.
false; // Not *not* false.
```
- Double-precision floats
- Integers
```lox
1234;  // An integer.
12.34; // A decimal number.
```
- Strings
```lox
"I am a string";
"";    // The empty string.
"123"; // This is a string, not a number.
```
- Nil
```lox
nil;
```

NOTE: The book spells it Nil to disambiguate it from the original host languages' null (Java and C).

3.4 Expressions

3.4.1 Arithemtic

```lox
add + me;
subtract - me;
multiply * me;
divide / me;

-negateMe;
```

NOTE: The + operator also supports string concatenation.

3.4.2 Comparison and equality

Comparison operators
```lox
less < than;
lessThan <= orEqual;
greater > than;
greaterThan >= orEqual;
```

Comparison of any value kind for equality or inequality is allowed.

```lox
1 == 2;         // false.
"cat" != "dog"; // true.
```

Comparison of different types is allowed.
```lox
314 == "pi"; // false.
```

Values of different types are never equivalent

```lox
123 == "123"; // false.
```

NOTE: No implicit conversion. "I’m generally against implicit conversions."

3.4.3 Logical Operators

Not, And, Or operators.

```lox
!true;  // false.
!false; // true.

true and false; // false.
true and true;  // true.

false or false; // false.
true or false;  // true.
```

Short-circuiting included.

3.4.4 Precedence and grouping

No bitwise, shift, modulo, or conditional operators in base lox language.

"All of these operators have the same precedence and associativity that you’d expect coming from C."

```lox
var average = (min + max) / 2;
```



\begin{code}

main :: IO ()
main = return ()
\end{code}
 