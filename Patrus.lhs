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

--alt style
--runFile fname = run =<< readFile fname
--main' [fname] = run =<< readFile fname
\end{code}
Source File: app/main.hs, after main'

NOTE: 11/6/21 So far the Alex lexer introduces `mtl` and `array` as dependencies.
Currently Lexer.x uses GHC.err error.

\begin{code}

runPrompt :: IO ()
runPrompt = do
    putStr "> "
    line <- getLine --MATCH? Does this need to be buffered?
    case line of
        "" -> return ()
        _  -> run line >> runPrompt

run :: String -> IO ()
run source = forM_ (scanTokens source) (\token -> putStrLn $ show token)

\end{code}
Source File: app/main.hs, after runFile'

4.1.1 Error Handling

NOTE: first instance of global state is hadError.

Hmm we might want to do Either String [Token] at this level to check and `ExitFailure 65`

\begin{code}



\end{code}