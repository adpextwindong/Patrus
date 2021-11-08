{     

module Patrus.Lexer where

import Control.Monad.State
import Control.Monad.Except
import Data.Word
}

$eol   = [\n]
tokens :-
    -- Whitespace insensitive
    $eol              ;

{

data Token =
     TTrue
     | TEOF
     deriving (Eq,Show)

-- TODO look at https://github.com/simonmar/alex/blob/master/data/AlexWrappers.hs

--https://github.com/jmoy/alexhappy/blob/master/arith1/Main.hs
-- The functions that must be provided to Alex's basic interface
type AlexInput = [Word8]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (b:bs) = Just (b,bs)
alexGetByte []    = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined


-- Our Parser monad
type P a = StateT AlexInput (Either String) a

evalP :: P a -> AlexInput -> Either String a
evalP = evalStateT

-- Action to read a token
readToken::P Token
readToken = do
      s <- get
      case alexScan s 0 of
            AlexEOF -> return TEOF
            AlexError _ -> throwError "!Lexical error"
            AlexSkip inp' _ -> do
                  put inp'
                  readToken
            AlexToken inp' _ token -> do
                  put inp'
                  return token

-- The lexer function to be passed to Happy
lexer :: (Token -> P a) -> P a
lexer cont = readToken >>= cont

}
