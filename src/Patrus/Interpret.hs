{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.Interpret where

import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, execStateT, modify')

import Patrus.Eval
import Patrus.Env
import Patrus.Types

import qualified Data.Map.Strict as M

runEvalM :: EvalM Expr -> Environment -> IO (Expr, Environment)
runEvalM x = runStateT (runEval x)

baseGlobalEnv :: Environment
baseGlobalEnv = Environment EmptyEnv (Env baseScope EmptyEnv) Nothing
    where baseScope = M.fromList [clock]
          clock = ("clock", NativeFunc Clock []) --impl in eval...

interpretProgram :: Program -> IO Environment
interpretProgram p = execStateT (runEval $ interpretM p) baseGlobalEnv

-- ((Return value, remainder of the program where it was returned), Enviornment)
-- interpretM and eval have now become intertwined.
-- Return statements will give back an expression (to be used by Eval)
-- Otherwise Unit is returned.
-- TODO think about this some more... 02:34 01-Jan-22
runProgram :: Program -> IO (Expr, Environment)
runProgram p = runStateT (runEval $ interpretM p) baseGlobalEnv
