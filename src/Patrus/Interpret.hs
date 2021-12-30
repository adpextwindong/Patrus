{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.Interpret where

import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, execStateT, modify')

import Patrus.Eval
import Patrus.Environment
import Patrus.Types

import qualified Data.Map.Strict as M

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

interpretM ((IfStatement conde trueBranch falseBranch): xs) = do
    e <- eval conde

    if literalTruth e
    then interpretM [trueBranch]
    else case falseBranch of
        Just fb -> interpretM [fb]
        Nothing -> pure []

    interpretM xs

interpretM w@((WhileStatement conde body):xs) = do
    e <- eval conde

    if literalTruth e
    then interpretM [body] >> interpretM w
    else interpretM xs

baseGlobalEnv = Env baseScope EmptyEnv
    where baseScope = M.fromList [clock]
          clock = ("clock", NativeFunc Clock []) --impl in eval...

interpretProgram :: Program -> IO Environment
interpretProgram p = execStateT (runEval $ interpretM p) baseGlobalEnv

runProgram :: Program -> IO (Program, Environment)
runProgram p = runStateT (runEval $ interpretM p) baseGlobalEnv
