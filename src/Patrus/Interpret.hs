{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.Interpret where

import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, execStateT, modify')
import Control.Monad.Except

import Patrus.Eval
import Patrus.Env
import Patrus.Types

import qualified Data.Map.Strict as M

runEvalM :: EvalM Expr -> Environment -> IO (Either EvalException Expr, Environment)
runEvalM x = runStateT (runExceptT (runEval x))

baseGlobalEnv :: Environment
baseGlobalEnv = Environment EmptyEnv (Env baseScope EmptyEnv) Nothing
    where baseScope = M.fromList [clock]
          clock = ("clock", NativeFunc Clock []) --impl in eval...

interpretProgram :: Program -> IO (Either EvalException Expr, Environment)
interpretProgram p = runEvalM (interpretM p) baseGlobalEnv

-- ((Return value, remainder of the program where it was returned), Enviornment)
-- interpretM and eval have now become intertwined.
-- Return statements will give back an expression (to be used by Eval)
-- Otherwise Unit is returned.
-- TODO think about this some more... 02:34 01-Jan-22
runProgram :: Program -> IO (Either EvalException Expr, Environment)
runProgram p = runStateT (runExceptT . runEval $ interpretM p) baseGlobalEnv
