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

--Hack around TEOF
TopLevel : Expr TEOF { $1 }

Expr : Equality { $1 }

Equality : Comparison BANG_EQUAL Equality       { BOp NEQ $1 $3 }
         | Comparison EQUAL_EQUAL Equality      { BOp AST.EQ $1 $3 }
         | Comparison                           { $1 }

Comparison : Term GREATER Term                  { BOp AST.GT $1 $3 }
           | Term GREATER_EQUAL Term            { BOp AST.GTE $1 $3 }
           | Term LESS Term                     { BOp AST.LT $1 $3 }
           | Term LESS_EQUAL Term               { BOp AST.LTE $1 $3 }
           | Term                               { $1 }

Term : Factor MINUS Factor                      { BOp Minus $1 $3 }
     | Factor PLUS Factor                       { BOp Plus $1 $3 }
     | Factor                                   { $1 }

Factor : Unary SLASH Unary                      { BOp Div $1 $3 }
       | Unary STAR Unary                       { BOp Mul $1 $3 }
       | Unary                                  { $1 }

Unary : BANG Unary                              { UOp Not $2 }
      | MINUS Unary                             { UOp Negate $2 }
      | Primary                                 { $1 }

Primary : TStringLiteral                        { (\(TStringLiteral s _) -> Lit (StringLit s)) $1 }
        | TNumberLiteral                        { (\(TNumberLiteral s _) -> Lit (NumberLit (read s))) $1 }
        | TRUE                                  { Lit TrueLit }
        | FALSE                                 { Lit FalseLit }
        | NIL                                   { Lit Nil }
        | LEFT_PAREN Expr RIGHT_PAREN           { Group $2 }

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
