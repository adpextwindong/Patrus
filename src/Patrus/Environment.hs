{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.Environment where

import qualified Data.Map.Strict as M
import Control.Monad.State.Class (MonadState (..))

import Patrus.Types

-- | Inserts the new variable into the outer most environment
insertEnv :: Identifier -> Expr -> Environment -> Environment
insertEnv i e EmptyEnv = Env (M.singleton i e) EmptyEnv
insertEnv i e (Env scope p) = Env (M.insert i e scope) p

-- | Adjusts an existing binding or fails if it does not exist in the lexical scopes.
adjustEnvFM :: (MonadFail m) => Identifier -> Expr -> Environment -> m Environment
adjustEnvFM i _ EmptyEnv = fail $ "Undefined variable \'" <> i <> "\'."
adjustEnvFM i e (Env scope p) = do
    if M.member i scope
    then return (Env (M.insert i e scope) p)
    else do
        p' <- adjustEnvFM i e p
        return $ Env scope p'

-- | Recursively looks up the identifier in each map
lookupEnv :: Identifier -> Environment -> Maybe Expr
lookupEnv i EmptyEnv = Nothing
lookupEnv i (Env scope parent) = case M.lookup i scope of
                                    Just v -> Just v
                                    Nothing -> lookupEnv i parent

pushFreshEnv :: Environment -> Environment
pushFreshEnv parentEnv = Env M.empty parentEnv

popEnv :: Environment -> Environment
popEnv (Env _ p) = p

withFreshEnv :: EvalM a -> EvalM ()
withFreshEnv f = modifyEnv (pushFreshEnv) >> f >> modifyEnv (popEnv)

pushFuncEnv :: [(Identifier, Expr)] -> Environment -> Environment
pushFuncEnv bindings env = Env (M.fromList bindings) env

withFuncEnv :: [(Identifier, Expr)] -> EvalM a -> EvalM a
withFuncEnv bindings f = do
    modifyEnv $ pushFuncEnv bindings
    e <- f
    modifyEnv popEnv
    return e

modifyEnv :: (Environment -> Environment) -> EvalM ()
modifyEnv f = get >>= (put . f)
