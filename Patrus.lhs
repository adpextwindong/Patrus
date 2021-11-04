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

\begin{code}

main :: IO ()
main = return ()
\end{code}
 