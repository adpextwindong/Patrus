{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Patrus.Parser
    (parseProgram, parseProgram') where

import Control.Monad.Except
import Control.Exception
import Data.Tuple.Extra (uncurry3)

import Patrus.Lexer
import Patrus.Types as AST

}

--Entry Point

%name statementsParse

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
    DUMP { TKeyword DUMP _ }

    TIdentifier     { TIdentifier _ _ }
    TEOF            { TEOF }
   %%

-- Production Rules

Program :: { Program }
Program : Declarations TEOF { $1 }

Declarations :: { [Statement] }
Declarations : Declarations Declaration { $2 : $1 }
           | {- empty -} { [] }

Declaration :: { Statement }
Declaration : VarDecl                   { $1 }
            | Statement                 { $1 }

VarDecl :: {Statement }
VarDecl : VAR TIdentifier EQUAL Expr SEMICOLON            { (\(TIdentifier s _) e -> VarDeclaration s (Just e)) $2 $4 }
        | VAR TIdentifier SEMICOLON                       { (\(TIdentifier s _) -> VarDeclaration s Nothing) $2 }

Statement :: { Statement }
Statement : OpenStmt       { $1 }
          | ClosedStmt     { $1 }

OpenStmt : IF LEFT_PAREN Expr RIGHT_PAREN Statement                    { IfStatement $3 $5 Nothing   }
         | IF LEFT_PAREN Expr RIGHT_PAREN ClosedStmt ELSE OpenStmt     { IfStatement $3 $5 (Just $7) }
         | WHILE LEFT_PAREN Expr RIGHT_PAREN OpenStmt                  { WhileStatement $3 $5 }
         | ForStmtSugar OpenStmt { (uncurry3 desugarFor $1) $2 }

ClosedStmt : SimpleStmt                                                 { $1 }
           | IF LEFT_PAREN Expr RIGHT_PAREN ClosedStmt ELSE ClosedStmt  { IfStatement $3 $5 (Just $7) }
           | WHILE LEFT_PAREN Expr RIGHT_PAREN ClosedStmt               { WhileStatement $3 $5 }
           | ForStmtSugar ClosedStmt { (uncurry3 desugarFor $1) $2 }

ForStmtSugar :: { (Maybe Statement, Maybe Expr, Maybe Expr) }
ForStmtSugar : FOR LEFT_PAREN ForInitializer ForCond ForIncrement RIGHT_PAREN { ($3, $4, $5) }

ForInitializer :: { Maybe Statement }
ForInitializer : VarDecl        { Just $1 }
               | ExprStatement  { Just $1 }
               | SEMICOLON      { Nothing }

ForCond :: { Maybe Expr }
ForCond : Expr SEMICOLON { Just $1 }
        | SEMICOLON { Nothing }

ForIncrement :: { Maybe Expr }
ForIncrement : Expr         { Just $1 }
             | {-empty-}    { Nothing }

SimpleStmt : ExprStatement  { $1 }
           | PrintStatement { $1 }
           | Block          { $1 }
           | DumpStatement  { $1 }

Block : LEFT_BRACE Declarations RIGHT_BRACE { BlockStatement (reverse $2) }

ExprStatement : Expr SEMICOLON                            { ExprStatement $1 }

PrintStatement : PRINT Expr SEMICOLON                     { PrintStatement $2 }

DumpStatement : DUMP SEMICOLON { DumpStatement }

Expr :: { Expr }
Expr : Assignment                               { $1 }

Assignment :: { Expr }
Assignment : TIdentifier EQUAL Assignment       { (\(TIdentifier s _) -> Assignment s $3 ) $1 }
           | Logic_Or                           { $1 }

Logic_Or : Logic_And OR Logic_Or  { BOp AST.Or $1 $3 }
         | Logic_And              { $1 }

Logic_And : Equality AND Logic_And  { BOp AST.And $1 $3 }
          | Equality                { $1 }

Equality :: { Expr }
Equality : Comparison BANG_EQUAL Equality       { BOp (Cmp NEQ) $1 $3 }
         | Comparison EQUAL_EQUAL Equality      { BOp (Cmp AST.EQ) $1 $3 }
         | Comparison                           { $1 }

Comparison :: { Expr }
Comparison : Term GREATER Term                  { BOp (Cmp AST.GT) $1 $3 }
           | Term GREATER_EQUAL Term            { BOp (Cmp AST.GTE) $1 $3 }
           | Term LESS Term                     { BOp (Cmp AST.LT) $1 $3 }
           | Term LESS_EQUAL Term               { BOp (Cmp AST.LTE) $1 $3 }
           | Term                               { $1 }

Term :: { Expr }
Term : Factor MINUS Factor                      { BOp Minus $1 $3 }
     | Factor PLUS Factor                       { BOp Plus $1 $3 }
     | Factor                                   { $1 }

Factor :: { Expr }
Factor : Unary SLASH Unary                      { BOp Div $1 $3 }
       | Unary STAR Unary                       { BOp Mul $1 $3 }
       | Unary                                  { $1 }

Unary :: { Expr }
Unary : BANG Unary                              { UOp Not $2 }
      | MINUS Unary                             { UOp Negate $2 }
      | Call                                    { $1 }

Call :: { Expr }
Call : Primary { $1 }
     | Primary Calls { $2 $1 }

--We pass in the identifier of the first callee then it will nest calls for foo()() correctly
Calls :: { Expr -> Expr }
Calls : LEFT_PAREN RIGHT_PAREN Calls            { \callee -> $3 (Call callee []) }
      | LEFT_PAREN Arguments RIGHT_PAREN Calls  { \callee -> $4 (Call callee $2) }
      | LEFT_PAREN RIGHT_PAREN                  { \callee -> Call callee [] }
      | LEFT_PAREN Arguments RIGHT_PAREN        { \callee -> Call callee $2 }

Arguments :: { [Expr] }
Arguments : Expr COMMA Arguments { $1 : $3 }
          | Expr { [$1] }

Primary :: { Expr }
Primary : TStringLiteral                        { (\(TStringLiteral s _) -> Lit (StringLit s)) $1 }
        | TNumberLiteral                        { (\(TNumberLiteral s _) -> Lit (NumberLit (read s))) $1 }
        | TRUE                                  { Lit (BoolLit True) }
        | FALSE                                 { Lit (BoolLit False) }
        | NIL                                   { Lit Nil }
        | LEFT_PAREN Expr RIGHT_PAREN           { Group $2 }
        | TIdentifier                           { (\(TIdentifier s _) -> Var s) $1 }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseProgram :: String -> Program
parseProgram s = case parseProgram' s of
    Left msg -> error ("Program parse error:" ++ msg)
    Right p -> reverse p

parseProgram' input = runExcept $ do
    statementsParse (scanTokens input)
}
