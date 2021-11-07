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
$alphanumeric = [_a-zA-Z0-9]
$eol   = [\n]

tokens :-
    -- Whitespace insensitive
    $eol              ;
    $white+           ;

    -- Comments
    "//".*            ;

    -- Symbols
    \(                { \p _ -> TSChar LPAREN p }
    \)                { \p _ -> TSChar RPAREN p }
    \{                { \p _ -> TSChar LBRACE p }
    \}                { \p _ -> TSChar RBRACE p }
    \,                { \p _ -> TSChar COMMA p }
    \.                { \p _ -> TSChar DOT p }
    \-                { \p _ -> TSChar MINUS p }
    \+                { \p _ -> TSChar PLUS p }
    \;                { \p _ -> TSChar SEMICOLON p }
    \/                { \p _ -> TSChar SLASH p }
    \*                { \p _ -> TSChar STAR p }

    -- Operators
    "!="              { \p _ -> TOp BANG_EQUAL p }
    "=="              { \p _ -> TOp EQUAL_EQUAL p }
    ">="              { \p _ -> TOp GREATER_EQUAL p }
    "<="              { \p _ -> TOp LESS_EQUAL p }
    "!"               { \p _ -> TOp BANG p }
    "="               { \p _ -> TOp EQUAL p }
    ">"               { \p _ -> TOp GREATER p }
    "<"               { \p _ -> TOp LESS p }

    -- Literals
    --TODO FIX MULTI LINE STRING LITERAL
    \" [a-zA-Z0-9]+ \" { \p s -> TStringLiteral s p }

    $digit+             { \p s -> TNumberLiteral s p }
    $digit+ \. $digit+  { \p s -> TNumberLiteral s p }

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

    $alpha $alphanumeric* { \p s -> TIdentifier s p }
{

--TODO impl Lox Lexer

data TSingleChar = LPAREN | RPAREN | LBRACE | RBRACE
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

scanTokens' :: String -> Except String [Token]
scanTokens' str = go (alexStartPos,'\n',[],str)
  where
    go inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF -> return [TEOF]
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
}