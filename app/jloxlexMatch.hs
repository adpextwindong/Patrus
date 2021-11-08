module Main where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (readFile)

import Patrus.Lexer (jloxlexMatch)

main :: IO ()
main = main' =<< getArgs

main' [fname] = jloxlexMatch =<< readFile fname
main' _ = putStrLn "Usage: jloxlexMatch ScriptPath"