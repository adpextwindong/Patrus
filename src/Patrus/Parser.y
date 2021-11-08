{
{-# LANGUAGE FlexibleContexts #-}
module Patrus.Parser where

import Patrus.AST
import qualified Patrus.Lexer as L
import Control.Monad.Except

}

%monad{L.P}
%lexer{L.lexer}{L.TEOF}
%name parse
%tokentype{L.Token}
%error {parseError}

%token
true            {L.TTrue}

%%
Term : true {STrue}

{

parseError _ = throwError "!Parse Error"

}