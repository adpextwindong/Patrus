{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.Env where

import qualified Data.Map.Strict as M
import Control.Monad.State.Class (MonadState (..))

import Patrus.Types

-- | Inserts the new variable into the outer most environment
insertEnv :: Identifier -> Expr -> Env -> Env
insertEnv i e EmptyEnv = Env (M.singleton i e) EmptyEnv
insertEnv i e (Env scope p) = Env (M.insert i e scope) p

insertEnvironment :: Identifier -> Expr -> Environment -> Environment
insertEnvironment i e (Environment EmptyEnv global rk) = Environment EmptyEnv (insertEnv i e global) rk
insertEnvironment i e (Environment env global rk) = Environment (insertEnv i e env) global rk

-- | Adjusts an existing binding or fails if it does not exist in the lexical scopes.
adjustEnvFM :: (MonadFail m) => Identifier -> Expr -> Env -> m Env
adjustEnvFM i _ EmptyEnv = fail $ "Undefined variable \'" <> i <> "\'."
adjustEnvFM i e (Env scope p) = do
    if M.member i scope
    then return (Env (M.insert i e scope) p)
    else do
        p' <- adjustEnvFM i e p
        return $ Env scope p'

adjustEnvironmentFM :: (MonadFail m) => Identifier -> Expr -> Environment -> m Environment
adjustEnvironmentFM i e (Environment EmptyEnv global k) = do
    global' <- adjustEnvFM i e global
    return (Environment EmptyEnv global k)

adjustEnvironmentFM i e (Environment env global k) = do
    env' <- adjustEnvFM i e env
    return (Environment env' global k)

-- | Recursively looks up the identifier in each map
lookupEnv :: Identifier -> Env -> Maybe Expr
lookupEnv i EmptyEnv = Nothing
lookupEnv i (Env scope parent) = case M.lookup i scope of
                                   Just v -> Just v
                                   Nothing -> lookupEnv i parent

pushFreshEnv :: Env -> Env
pushFreshEnv parentEnv = Env M.empty parentEnv

popEnv :: Env -> Env
popEnv (Env _ p) = p
popEnv EmptyEnv = EmptyEnv

popFuncEnvironment :: Environment -> Environment
popFuncEnvironment (Environment (Env _ p) global rk) = Environment p global rk
popFuncEnvironment (Environment EmptyEnv global rk) = Environment EmptyEnv global rk

withFreshEnv :: EvalM a -> EvalM a
withFreshEnv f = do
    modifyEnv $ pushFreshEnv
    e <- f
    modifyEnv popEnv
    return e

pushFuncEnv :: [(Identifier, Expr)] -> Env -> Env
pushFuncEnv bindings env = Env (M.fromList bindings) env

pushFuncEnvironment :: [(Identifier , Expr)] -> Environment -> Environment
pushFuncEnvironment bindings (Environment env global rk) = Environment (pushFuncEnv bindings env) global rk

withFuncEnv :: [(Identifier, Expr)] -> EvalM a -> EvalM a
withFuncEnv bindings f = do
    modifyEnv $ pushFuncEnv bindings
    e <- f
    modifyEnv popEnv
    return e

--TODO fix for global after next chapter
--I split the Environment into a lexical scope and another for globals (which should always be 1 lexical scope).
--I need to double check if modifyEnv respects that
modifyEnv :: (Env -> Env) -> EvalM ()
modifyEnv f = do
    environ@(Environment env global k) <- get
    put (Environment (f env) global k)
