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

Comparison : Term                               { $1 }
        --TODO term ops

Term : Factor                                   { $1 }
        --TODO ops

Factor : Unary                                  { $1 }
        --TODO ops

Unary : Primary                                 { $1 }
        --TODO ops

Primary : TStringLiteral                        { (\(TStringLiteral s _) -> Lit (StringLit s)) $1 }
        --TODO other literals

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
