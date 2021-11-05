A Tree-Walk Interpreter

NOTE: For now lets stick to base as much as possible and plain strings. Not sure about LazyIO either. Have to figure out the audience level for this translation.

4 - Scanning

4.1 - The Interpreter Framework

\begin{code}

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (readFile)

main :: IO ()
main = main' =<< getArgs

main' []      = runPrompt
main' [fname] = runFile fname
main' xs = do
    print "Usage: Patrus [script]"
    exitWith (ExitFailure 64)

\end{code}

\begin{code}
runFile :: FilePath -> IO ()
runFile fname = do
    fileContents <- readFile fname
    run fileContents

--alt style
--runFile fname = run =<< readFile fname
--main' [fname] = run =<< readFile fname
\end{code}

\begin{code}

runPrompt :: IO ()
runPrompt = undefined --TODO REPL


run = undefined

\end{code}