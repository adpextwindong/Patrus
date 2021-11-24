A Tree-Walk Interpreter

NOTE: For now lets stick to base as much as possible and plain strings. Not sure about LazyIO either. Have to figure out the audience level for this translation.

4 - Scanning

4.1 - The Interpreter Framework

\begin{code}
module Patrus where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (readFile)
import Control.Monad (forM_)

import Debug.Trace (trace)

import Patrus.Lexer (Token(..), scanTokens)

todo s = trace ("TODO: " ++ s) undefined

main :: IO ()
main = main' =<< getArgs

main' []      = runPrompt
main' [fname] = runFile fname
main' xs = do
    putStrLn "Usage: Patrus [script]"
    exitWith (ExitFailure 64) --EX_USAGE

\end{code}
Source File: app/main.hs, create new file

\begin{code}
runFile :: FilePath -> IO ()
runFile fname = do
    fileContents <- readFile fname
    run fileContents

\end{code}
Source File: app/main.hs, after main'

NOTE: 11/6/21 So far the Alex lexer introduces `mtl` and `array` as dependencies.
Currently Lexer.x uses GHC.err error.

\begin{code}

runPrompt :: IO ()
runPrompt = do
    putStr "> "
    line <- getLine
    case line of
        "" -> return ()
        _  -> run line >> runPrompt

run :: String -> IO ()
run source = forM_ (scanTokens source) (\token -> putStrLn $ show token)

\end{code}
Source File: app/main.hs, after runFile'

4.1.1 - Error Handling

NOTE: first instance of global state is hadError.

Hmm we might want to do Either String [Token] at this level to check and `ExitFailure 65`

```haskell
scanTokens' :: String -> Except String [Token]
scanTokens' str = go (alexStartPos,'\n',[],str)
  where
    go inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF -> return []
        AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
        AlexSkip  inp' _       -> go inp'
        AlexToken inp' len act -> do
          res <- go inp'
          let rest = act pos (take len str)
          return (rest : res)

--TODO replace this once we figure out if Control.Monad.Except can hang around or we ditch Alex
scanTokens :: String -> [Token]
scanTokens s = case (runExcept (scanTokens' s)) of
                    Left error_str -> error error_str
                    Right ts -> ts
```
Source File: src/Patrus/Lexer.x, Line 38

4.2 - Lexems and Tokens

```lox
var language = "lox";
```

4.2.1 Token type

```haskell
data TSingleChar = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
                 | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR
  deriving Show

data TOperator = BANG | BANG_EQUAL |
                EQUAL | EQUAL_EQUAL |
                GREATER | GREATER_EQUAL |
                LESS | LESS_EQUAL
  deriving Show

data TKeyword = AND | CLASS | ELSE | FALSE | FUN | FOR |  IF | NIL | OR | 
                PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE
  deriving Show

data Token = TSChar TSingleChar AlexPosn
           | TOp TOperator AlexPosn
           | TStringLiteral String AlexPosn
           | TNumberLiteral String AlexPosn
           | TIdentifier String AlexPosn
           | TKeyword TKeyword AlexPosn
           | TEOF
    deriving Show
```
Source File: src/Patrus/Lexer.x, Line 24 before scanTokens'

For line position information we'll just use AlexPosn for now which looks like this

```haskell
data AlexPosn = AlexPn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number
```
From the [Alex docs](https://www.haskell.org/alex/doc/html/wrappers.html#id462357).

Note: Skipping toString() for now and just using the Show instance.

4.4 Scanner Class

TODO make sure EOF gets lexed.

4.5 Recognizing Lexems

```alex
{
{-# LANGUAGE FlexibleContexts #-}

module Patrus.Lexer (
    Token(..), TKeyword(..), TOperator(..), TSingleChar(..),
    scanTokens
) where

import Control.Monad.Except
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
    -- Whitespace insensitive
    $eol              ;
    $white+           ;

    -- Comments
    "//".*            ;

    -- Symbols
    \(                { \p _ -> TSChar LEFT_PAREN p }
    \)                { \p _ -> TSChar RIGHT_PAREN p }
    \{                { \p _ -> TSChar LEFT_BRACE p }
    \}                { \p _ -> TSChar RIGHT_BRACE p }
    \,                { \p _ -> TSChar COMMA p }
    \.                { \p _ -> TSChar DOT p }
    \-                { \p _ -> TSChar MINUS p }
    \+                { \p _ -> TSChar PLUS p }
    \;                { \p _ -> TSChar SEMICOLON p }
    \/                { \p _ -> TSChar SLASH p }
    \*                { \p _ -> TSChar STAR p }
```
Source File: src/Patrus/Lexer.x, start of file before TSingleChar datatype declaration.

4.5.1 - Lexical errors

See 4.1.1 notes

4.5.2 - Operators

```alex
    -- Operators
    "!="              { \p _ -> TOp BANG_EQUAL p }
    "=="              { \p _ -> TOp EQUAL_EQUAL p }
    ">="              { \p _ -> TOp GREATER_EQUAL p }
    "<="              { \p _ -> TOp LESS_EQUAL p }
    "!"               { \p _ -> TOp BANG p }
    "="               { \p _ -> TOp EQUAL p }
    ">"               { \p _ -> TOp GREATER p }
    "<"               { \p _ -> TOp LESS p }
```
Source File: src/Patrus/Lexer.x, line 39 after symbols block.

4.6 - Longer Lexemes

Note: Alex handles this slash handling for us already.

TODO fix EOF handling incase parser needs it.
TODO check how jlox lexes "\0" vs "+".

So far it should be able to handle code like this.

```lox
// this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == // operators
```

4.6.1 - String Literals

NOTE: "For no particular reason, Lox supports multi-line strings."

NOTE: "Finally, the last interesting bit is that when we create the token, we also produce the actual string value that will be used later by the interpreter. Here, that conversion only requires a substring() to strip off the surrounding quotes. If Lox supported escape sequences like \n, we’d unescape those here."

TODO unterminated string error.
TODO multiline string lexing

For now we'll use this.
```alex
    -- Literals
    \" [a-zA-Z0-9\n]* \" { \p s -> TStringLiteral s p }
```
Source File: src/Patrus/Lexer.x, Line 50 after operators regular expressions

4.6.2 - Number literals

```alex
    $digit+             { \p s -> TNumberLiteral s p }
    $digit+ \. $digit+  { \p s -> TNumberLiteral s p }
```
Source File: src/Patrus/Lexer.x, Line 54 after string literal regular expression.

4.7 - Reserved Words and Identifiers

We'll add an alpha numeric macro to make this next bit easier.

```alex
$alphanumeric = [_a-zA-Z0-9]
```
Source File: src/Patrus/Lexer.x, Line 16 after alpha macro.

```alex
    $alpha $alphanumeric+ { \p s -> TIdentifier s p }
```
Source File: src/Patrus/Lexer.x, Line 57 after number literal regular expression.

```alex
    "and"                { \p s -> TKeyword AND p }
    "class"              { \p s -> TKeyword CLASS p }
    "else"               { \p s -> TKeyword ELSE p }
    "false"              { \p s -> TKeyword FALSE p }
    "for"                { \p s -> TKeyword FOR p }
    "fun"                { \p s -> TKeyword FUN p }
    "if"                 { \p s -> TKeyword IF p }
    "nil"                { \p s -> TKeyword NIL p }
    "or"                 { \p s -> TKeyword OR p }
    "print"              { \p s -> TKeyword PRINT p }
    "return"             { \p s -> TKeyword RETURN p }
    "super"              { \p s -> TKeyword SUPER p }
    "this"               { \p s -> TKeyword THIS p }
    "true"               { \p s -> TKeyword TRUE p }
    "var"                { \p s -> TKeyword VAR p }
    "while"              { \p s -> TKeyword WHILE p }
```
Source File: src/Patrus/Lexer.x, Line 57 BEFORE identifier regular expression.

NOTE: PLACE THE KEYWORD REGULAR EXPRESSIONS *BEFORE* THE IDENTIFER REGULAR EXPRESSION TO MAKE SURE THEY DON'T GET MISTAKENLY LEXED AS IDENTIFER TOKENS.

NOTE: For the time being it doesn't seem like Lox has escaped characters in string literals so adding '\n' seems to patch over the multi-line string literal issue. `jlox --lex`` shows that it'll skip over the error and continue lexing but a file ran with just `jlox` won't be interpreted if theres a lexical error in any of the lines.
TODO: write a test show casing this behavior.

NOTE: "Our interpreter uses Java’s Double type to represent numbers"

5.1.3

Lox's Grammar

```
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
```

5.2 - Implementing Syntax Trees

Untyped Expr example.

```haskell
data BinOp = EQ | NEQ | LT | LTE | GT | GTE
           | Plus | Minus | Mul | Div

data UnaryOp = Negate | Not

data Literal = NumberLit Double
             | StringLit String
             | TrueLit
             | FalseLit
             | Nil

data Expr = BOp BinOp Expr Expr
          | UOp UnaryOp Expr
          | Lit Literal
          | Group Expr
```
Source File: src/Patrus/AST.hs, create new file

5.2.2 - Metaprogramming the trees

"Java can express behavior-less classes, but I wouldn’t say that it’s particularly great at it. Eleven lines of code to stuff three fields in an object is pretty tedious, and when we’re all done, we’re going to have 21 of these classes."

YIKES.

"[Appendix II](https://craftinginterpreters.com/appendix-ii.html) contains the code generated by this script once we’ve finished implementing jlox and defined all of its syntax tree nodes."

5.3.2 - The Visitor Pattern

YIKES.

5.4 - A (Not Very) Pretty Printer

```haskell
wrapParens :: String -> String
wrapParens s = "(" ++ s ++ ")"

prettyPrintAST :: Expr -> String
prettyPrintAST (BOp op e1 e2)   = wrapParens $ prettyPrintBop op ++ " " ++ prettyPrintAST e1 ++ " " ++ prettyPrintAST e2
prettyPrintAST (UOp op e)       = wrapParens $ prettyPrintUop op ++ prettyPrintAST e
prettyPrintAST (Group e)        = wrapParens $ "group " ++ prettyPrintAST e
prettyPrintAST (Lit v)          = prettyPrintLit v

prettyPrintLit :: Literal -> String
prettyPrintLit (NumberLit v)    = formatFloat v
prettyPrintLit (StringLit s)    = show s
prettyPrintLit (TrueLit)        = show True
prettyPrintLit (FalseLit)       = show False
prettyPrintLit (Nil)            = "nil"

prettyPrintBop :: BinOp -> String
prettyPrintBop (EQ)     = "=="
prettyPrintBop (NEQ)    = "!="
prettyPrintBop (LT)     = "<"
prettyPrintBop (LTE)    = "<="
prettyPrintBop (GT)     = ">"
prettyPrintBop (GTE)    = ">="
prettyPrintBop (Plus)   = "+"
prettyPrintBop (Minus)  = "-"
prettyPrintBop (Mul)    = "*"
prettyPrintBop (Div)    = "/"

prettyPrintUop :: UnaryOp -> String
prettyPrintUop (Negate) = "-"
prettyPrintUop (Not)    = "!"

--jlox formats Doubles different than Haskell's show.
--https://stackoverflow.com/a/35980995
formatFloat :: RealFloat a => a -> String
formatFloat v
  | v == 0                          = "0"
  | abs v < 1e-5 || abs v > 1e10    = formatRealFloat FFExponent Nothing v
  | v - fromIntegral (floor v) == 0 = formatRealFloat FFFixed   (Just 0) v
  | otherwise                       = formatRealFloat FFGeneric  Nothing v
```
Source File: src/AST.hs, add after Expr definition.

```haskell
main = do
    let expression = BOp Mul (UOp Negate (Lit (NumberLit 123))) (Group (Lit (NumberLit 45.67)))
    putStrLn $ prettyPrintAST expression

-- (* (- 123) (group 45.67))
```

# 6 - Parsing Expressions

```
6 / 3 - 1
```

```
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
```

With this it could be parsed as either

```
(6 / (3 - 1))
```
or
```
((6/3) - 1)
```

The parsing paths respectively being:

Start at expression
1. Pick binary.
    - For the left expression, pick NUMBER 6
    - For the operator, pick operator /
    - For the right expression, pick binary
        - pick number 3 for left expression
        - pick operator -
        - pick number 1 for right expression

The other parse path starting at expression being:
1. Pick binary,
    - For the left expression, pick binary
        - Pick number 6
        - pick operator /
        - pick number 3
    - For the operator pick -
    - For the right expresion, pick number 1

The culprit being the binary rule in expression. It allows for operand nesting in any way.

To combat this:

- Precedence, "Higher precedence binds tighter"
- Associativity, (5 - 3 - 1) => ((5 - 3) - 1)

Lox Precedence Rules

Lowest to highest in terms of precedence rules.

|Name|Operators|Associates|
|-|-|-|
|Equality|== !=|Left|
|Comparison|> >= < <=|Left|
|Term|- +|Left|
|Factor|/ *|Left|
|Unary|! -|Right|


Original Grammar for the math expression example.
```
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" exnary          → pression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
```

Instead of stuffing everything into a single expression rule we can define a seperate rule for each precedence level, stratifying the gammar.

```
expression     → ...
equality       → ...
comparison     → ...
term           → ...
factor         → ...
unary          → ...
primary        → ...
```

Primary covers the highest-precedence forms, literals and parenthesized expressions. Term can match `1 + 2` and `3 * 4 / 5`.

So we pretty much lifted literals and unary out to higher precedence levels.

Now we have well defined precedence and associativity to prevent ambiguity during parsing.

```
expression  -> equality ;
equality    -> comparison ( ( "!=" | "==" ) comparison )* ;
comparision -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term        -> facator ( ( "-" | "+" ) factor )* ;
factor      -> unary ( ( "/" | "*" ) unary )* ;
unary       -> ( "!" | "*" ) unary )* ;
            | primary ;
primary     -> NUMBER | STRING | "true" | "false" | "nil"
            | "(" expression ")" ;
```

Unary being right recursive handles nesting like !!true. Precedence of primary prevents it from not terminating.

--TODO look at the appendix grammar

Begin the parser by copying over token definitions from Lexer.x

It should look like this.

```
{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Patrus.Parser where

import Control.Monad.Except
import Control.Exception

import Patrus.Lexer
import Patrus.AST as AST

}

--Entry Point

%name expressionParse

%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names

%token
    --TSingleChar
    LEFT_PAREN      { TSChar LEFT_PAREN _ }

    RIGHT_PAREN     { TSChar RIGHT_PAREN _ }
    LEFT_BRACE      { TSChar LEFT_BRACE _ }
    RIGHT_BRACE     { TSChar RIGHT_BRACE _ }
    COMMA           { TSChar COMMA _ }
    DOT             { TSChar DOT _ }
    MINUS           { TSChar MINUS _ }
    PLUS            { TSChar PLUS _ }
    SEMICOLON       { TSChar SEMICOLON _ }
    SLASH           { TSChar SLASH _ }
    STAR            { TSChar STAR _ }

    -- Operators
    BANG_EQUAL      { TOp BANG_EQUAL _ }
    EQUAL_EQUAL     { TOp EQUAL_EQUAL _ }
    GREATER_EQUAL   { TOp GREATER_EQUAL _ }
    LESS_EQUAL      { TOp LESS_EQUAL _ }
    BANG            { TOp BANG _ }
    EQUAL           { TOp EQUAL _ }
    GREATER         { TOp GREATER _ }
    LESS            { TOp LESS _ }

    -- Literals
    TStringLiteral  { TStringLiteral _ _ }
    TNumberLiteral  { TNumberLiteral _ _ }

    AND             { TKeyword AND _ }
    CLASS           { TKeyword CLASS _ }
    ELSE            { TKeyword ELSE _ }
    FALSE           { TKeyword FALSE _ }
    FOR             { TKeyword FOR _ }
    FUN             { TKeyword FUN _ }
    IF              { TKeyword IF _ }
    NIL             { TKeyword NIL _ }
    OR              { TKeyword OR _ }
    PRINT           { TKeyword PRINT _ }
    RETURN          { TKeyword RETURN _ }
    SUPER           { TKeyword SUPER _ }
    THIS            { TKeyword THIS _ }
    TRUE            { TKeyword TRUE _ }
    VAR             { TKeyword VAR _ }
    WHILE           { TKeyword WHILE _ }

    TIdentifier     { TIdentifier _ _ }
    TEOF            { TEOF }
   %%

-- Production Rules

Equality : { undefined }
Expr : Equality { $1 }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpression :: String -> Expr
parseExpression s = case parseExpression' s of
                Left msg -> error ("parse error:" ++ msg)
                Right e -> e

parseExpression' input = runExcept $ do
    expressionParse (scanTokens input)

}
```
Source File: src/Patrus/Parser.y, create new file

For the time being the Equality production rule returns undefined so we can type check the rest.

One thing to note about Happy is that each rule does have a return type so you can use that to typecheck your production rule expressions. Another thing to note is that each production term after the colon is associated with a number starting a 1.

For example

```
Equality : Comparison BANG_EQUAL Comparison    { BOp NEQ $1 $3 }
         | Comparison EQUAL_EQUAL Comparison   { BOp AST.EQ $1 $3 }
         | Comparison                          { $1 }

Comparison : { undefined }
```
Source File: src/Patrus/Parser.y, add after Expr production rule

```
 data BinOp = EQ | NEQ | LT | LTE | GT | GTE
            | Plus | Minus | Mul | Div
+           deriving Show

 data UnaryOp = Negate | Not
+    deriving Show

 data Literal = NumberLit Double
              | StringLit String
              | TrueLit
              | FalseLit
              | Nil
+                deriving Show

 data Expr = BOp BinOp Expr Expr
           | UOp UnaryOp Expr
           | Lit Literal
           | Group Expr
+            deriving Show
```
Source File: src/Patrus/AST.hs, add Show instances.

For the sake of simple testing we'll add Show instances to the current AST.

For working around the end of file token for now we'll have a top most production rule that dispatches to Expr and consumes the TEOF token.

```
--Hack around TEOF
TopLevel : Expr TEOF { $1 }
```
Source File: src/Patrus/Parser.y, add before Expr production rule.

Additionally to test that equality is being parsed lets fill out the remaining production rules just enough to test string literal equality parsing.

```
-Equality : Comparison BANG_EQUAL Comparison     { BOp NEQ $1 $3 }
-         | Comparison EQUAL_EQUAL Comparison    { BOp AST.EQ $1 $3 }
+Equality : Comparison BANG_EQUAL Equality       { BOp NEQ $1 $3 }
+         | Comparison EQUAL_EQUAL Equality      { BOp AST.EQ $1 $3 }
          | Comparison                           { $1 }

-Comparison : { undefined }
+Comparison : Term                               { $1 }
+        --TODO term ops
+
+Term : Factor                   { $1 }
+        --TODO ops
+
+Factor : Unary                                  { $1 }
+        --TODO ops

+Unary : Primary                                 { $1 }
+        --TODO ops

+Primary : TStringLiteral                        { (\(TStringLiteral s _) -> Lit (StringLit s)) $1 }
+        --TODO other literals
```
Source File: src/Patrus/Parser.y, modify after Expr.

Now we can test parseExpression.

```
parseExpression "\"ddd\" == \"aa\""

BOp EQ (Lit (StringLit "\"ddd\"")) (Lit (StringLit "\"aa\""))
```

Heres another example

```
parseExpression "\"ddd\" == \"aa\" == \"foo\""

BOp EQ (Lit (StringLit "\"ddd\"")) (BOp EQ (Lit (StringLit "\"aa\"")) (Lit (StringLit "\"foo\"")))
```

So for now things look good.

Lets finish out the remaining operators for the production rules.

```
-Comparison : Term                               { $1 }
-        --TODO term ops
+Comparison : Term GREATER Term                  { BOp AST.GT $1 $3 }
+           | Term GREATER_EQUAL Term            { BOp AST.GTE $1 $3 }
+           | Term LESS Term                     { BOp AST.LT $1 $3 }
+           | Term LESS_EQUAL Term               { BOp AST.LTE $1 $3 }
+           | Term                               { $1 }

-Term : Factor                                   { $1 }
-        --TODO ops
+Term : Factor MINUS Factor                      { BOp Minus $1 $3 }
+     | Factor PLUS Factor                       { BOp Plus $1 $3 }
+     | Factor                                   { $1 }

-Factor : Unary                                  { $1 }
-        --TODO ops
+Factor : Unary SLASH Unary                      { BOp Div $1 $3 }
+       | Unary STAR Unary                       { BOp Mul $1 $3 }
+       | Unary                                  { $1 }

-Unary : Primary                                 { $1 }
-        --TODO ops
+Unary : BANG Unary                              { UOp Not $2 }
+      | MINUS Unary                             { UOp Negate $2 }
+      | Primary                                 { $1 }

 Primary : TStringLiteral                        { (\(TStringLiteral s _) -> Lit (StringLit s)) $1 }
-        --TODO other literals
+        | TNumberLiteral                        { (\(TNumberLiteral s _) -> Lit (NumberLit (read s))) $1 }
+        | TRUE                                  { Lit TrueLit }
+        | FALSE                                 { Lit FalseLit }
+        | NIL                                   { Lit Nil }
+        | LEFT_PAREN Expr RIGHT_PAREN           { Group $2 }
```
Source File: src/Patrus/Parser.y, add remaining production rules for the 6.1 grammar.

```
parseExpression "6 /3 - 1"

BOp Minus (BOp Div (Lit (NumberLit 6.0)) (Lit (NumberLit 3.0))) (Lit (NumberLit 1.0)

parseExpression "6 / (3 - 1)"

BOp Div (Lit (NumberLit 6.0)) (Group (BOp Minus (Lit (NumberLit 3.0)) (Lit (NumberLit 1.0))))
```

The rules parse as we'd like.

-- 6.3 Syntax Errors

Happy has an error token and partial directive.

Jlox does support parsing after an error to at least show further syntax errors. For example

```
> var foo = 3; var bar = 3f; var baz = "quux"; var tx = 3f;
[line 1] Error at 'f': Expect ';' after variable declaration.
[line 1] Error at 'f': Expect ';' after variable declaration.
```

We'll need to figure out some way to synchronize the Happy parser to resume parsing to produce further syntax errors.

NOTE: "Alas, we don’t get to see this method in action, since we don’t have statements yet. We’ll get to that in a couple of chapters. For now, if an error occurs, we’ll panic and unwind all the way to the top and stop parsing. Since we can parse only a single expression anyway, that’s no big loss."

So using parseExpression' :: String -> Either String [Expr] to handle errors will be nice enough for now.
