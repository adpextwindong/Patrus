module Patrus.Interpret where

import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, execStateT, modify')

import Patrus.Eval
import Patrus.Environment
import Patrus.Types

runEvalM :: EvalM Expr -> Environment -> IO (Expr, Environment)
runEvalM x = runStateT (runEval x)

interpretM :: Program -> EvalM Program
interpretM [] = return []
interpretM ((PrintStatement e): xs) = do
    e' <- eval e
    liftIO $ print $ "PRINT: " <> show e'
    interpretM xs

interpretM ((DumpStatement) : xs) = do
    env <- get
    liftIO $ print $ "DUMP: " <> show env
    interpretM xs

interpretM ((ExprStatement e) : xs) = do
    _ <- eval e
    interpretM xs

interpretM ((VarDeclaration i Nothing) : xs) = do
    modifyEnv $ insertEnv i (Lit Nil)
    interpretM xs

interpretM (VarDeclaration i (Just e) : xs) = do
    e' <- eval e
    modifyEnv (insertEnv i e')
    interpretM xs

interpretM ((BlockStatement bs):xs) = do
    withFreshEnv (interpretM bs)
    interpretM xs

interpretProgram :: Program -> IO Environment
interpretProgram p = execStateT (runEval $ interpretM p) EmptyEnv

runProgram :: Program -> IO (Program, Environment)
runProgram p = runStateT (runEval $ interpretM p) EmptyEnv
