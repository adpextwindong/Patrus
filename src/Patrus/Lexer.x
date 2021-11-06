{
{-# LANGUAGE FlexibleContexts #-}

module Patrus.Lexer (
    Token(..),
    scanTokens
) where

import Control.Monad.Except
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+ ;

{

--TODO impl Lox Lexer
data Token = Token
    deriving Show

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _ lineNum _) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _ _ colNum) = colNum

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
}