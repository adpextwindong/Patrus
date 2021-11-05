# Section 1 Notes

## 1 - Introduction

### 1.3 - The First Interpreter

"We’ll write the simplest, cleanest code we can to correctly implement the semantics of the language."

## 2 - A Map of the Territory

#### 2.1.1 - Scanning

For now I'll use Alex to write the Lexer.

#### 2.1.2 - Parsing

For now I'll use Happy to generate a parser.

#### 2.1.3 - Static Analysis

"The language we’ll build in this book is dynamically typed, so it will do its type checking later, at runtime."

#### 2.2.2 - Tree-walk interpreters

"Some programming languages begin executing code right after parsing it to an AST (with maybe a bit of static analysis applied). To run the program, the interpreter traverses the syntax tree one branch and leaf at a time, evaluating each node as it goes."

"A notable exception is early versions of Ruby, which were tree walkers. At 1.9, the canonical implementation of Ruby switched from the original MRI (Matz’s Ruby Interpreter) to Koichi Sasada’s YARV (Yet Another Ruby VM). YARV is a bytecode virtual machine."

## 3 - The Lox Language

### 3.1 - Hello, Lox

```lox
// Your first Lox program!
print "Hello, world!";
```

### 3.2 - A High-Level Language

"As we’ll learn later, Lox’s approach to scoping hews closely to Scheme."

### 3.3 - Data Types

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

### 3.4 - Expressions

#### 3.4.1 - Arithemtic

```lox
add + me;
subtract - me;
multiply * me;
divide / me;

-negateMe;
```

NOTE: The + operator also supports string concatenation.

#### 3.4.2 - Comparison and equality

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

#### 3.4.3 - Logical Operators

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

#### 3.4.4 - Precedence and grouping

No bitwise, shift, modulo, or conditional operators in base lox language.

"All of these operators have the same precedence and associativity that you’d expect coming from C."

```lox
var average = (min + max) / 2;
```

### 3.5 - Statements

"An expression followed by a semicolon (;) promotes the expression to statement-hood."

"Baking print into the language instead of just making it a core library function is a hack. But it’s a useful hack for us: it means our in-progress interpreter can start producing output before we’ve implemented all of the machinery required to define functions, look them up by name, and call them."

```lox
print "Hello, world!";

"some expression";

{
  print "One statement.";
  print "Two statements.";
}
```

### 3.6 - Variables

NOTE: "If you omit the initializer, the variable’s value defaults to nil."

Rationale: "This is one of those cases where not having nil and forcing every variable to be initialized to some value would be more annoying than dealing with nil itself."

"[variable scope rules] works like you would expect coming from C or Java."

```lox
var imAVariable = "here is my value";
var iAmNil;

var breakfast = "bagels";
print breakfast; // "bagels".
breakfast = "beignets";
print breakfast; // "beignets".
```

### 3.7 - Control Flow

Note: "do while" ommited.

```lox
if (condition) {
  print "yes";
} else {
  print "no";
}

var a = 1;
while (a < 10) {
  print a;
  a = a + 1;
}

for (var a = 1; a < 10; a = a + 1) {
  print a;
}
```

### 3.8 - Functions

C style funcall syntax.

NOTE: "If execution reaches the end of the block without hitting a return, it implicitly returns nil."

```
makeBreakfast(bacon, eggs, toast);

makeBreakfast();

fun printSum(a, b) {
  print a + b;
}

fun returnSum(a, b) {
  return a + b;
}
```

#### 3.8.1 - Closures

First class functions

```lox
fun addPair(a, b) {
  return a + b;
}

fun identity(a) {
  return a;
}

print identity(addPair)(1, 2); // Prints "3".
```

Local function declaration

```lox
fun outerFunction() {
  fun localFunction() {
    print "I'm local!";
  }

  localFunction();
}
```

Local functions + First Class Functions + Block Scope

```
fun returnFunction() {
  var outside = "outside";

  fun inner() {
    print outside;
  }

  return inner;
}

var fn = returnFunction();
fn();
```

Note: fn must retain a binding to variable `outside` for when it is called later. Therefore inner must "close over" `outside`.

"These days, the term is often used for any first-class function, though it’s sort of a misnomer if the function doesn’t happen to close over any variables.

As you can imagine, implementing these adds some complexity because we can no longer assume variable scope works strictly like a stack where local variables evaporate the moment the function returns."

### 3.9 - Classes

#### 3.9.4 - Classes in Lox

Class Declaration

```
class Breakfast {
  cook() {
    print "Eggs a-fryin'!";
  }

  serve(who) {
    print "Enjoy your breakfast, " + who + ".";
  }
}
```

"The body of a class contains its methods. They look like function declarations but without the fun keyword. When the class declaration is executed, Lox creates a class object and stores that in a variable named after the class. Just like functions, classes are first class in Lox."

Classes are first class

```lox
// Store it in variables.
var someVariable = Breakfast;

// Pass it to functions.
someFunction(Breakfast);
```

Create new class instance by function calling on class name. This avoids introducing another "new" keyword.

```lox
var breakfast = Breakfast();
print breakfast; // "Breakfast instance".
```

#### 3.9.5 - Instantiation and initialization

Assigning to a non-existing field will create it on the instance.

```lox
breakfast.meat = "sausage";
breakfast.bread = "sourdough";
```

This keyword

```lox
class Breakfast {
  serve(who) {
    print "Enjoy your " + this.meat + " and " +
        this.bread + ", " + who + ".";
  }

  // ...
}
```

Class constructor can be defined using init([PARAMS])

```lox
class Breakfast {
  init(meat, bread) {
    this.meat = meat;
    this.bread = bread;
  }

  // ...
}

var baconAndToast = Breakfast("bacon", "toast");
baconAndToast.serve("Dear Reader");
// "Enjoy your bacon and toast, Dear Reader."
```

#### 3.9.6 - Inheritance

Single Inheritance

```lox
class Brunch < Breakfast {
  drink() {
    print "How about a Bloody Mary?";
  }
}
```

"If you know any type theory, you’ll notice it’s not a totally arbitrary choice. Every instance of a subclass is an instance of its superclass too, but there may be instances of the superclass that are not instances of the subclass. That means, in the universe of objects, the set of subclass objects is smaller than the superclass’s set, though type nerds usually use <: for that relation."

Superclass methods are avalible to its subclasses

```lox
var benedict = Brunch("ham", "English muffin");
benedict.serve("Noble Reader");
```

Superclass constructor also gets inherited. This leads us to `super` keyword like in Java.

"Lox is different from C++, Java, and C#, which do not inherit constructors, but similar to Smalltalk and Ruby, which do."

```lox
class Brunch < Breakfast {
  init(meat, bread, drink) {
    super.init(meat, bread);
    this.drink = drink;
  }
}
```

#### 3.10 - The Standard Library

We'll have a built-in print statement and eventually a clock function for benchmarking.

 