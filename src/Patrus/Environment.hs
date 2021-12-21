module Patrus.Environment where

import qualified Data.Map.Strict as M
import Control.Monad.State.Class (MonadState (..))

import Patrus.Types

insertEnv :: Identifier -> Expr -> Environment -> Environment
insertEnv i e EmptyEnv = Env (M.singleton i e) EmptyEnv
insertEnv i e (Env scope p) = Env (M.insert i e scope) p

lookupEnv :: Identifier -> Environment -> Maybe Expr
lookupEnv i EmptyEnv = Nothing
lookupEnv i (Env scope parent) = case M.lookup i scope of
                                    Just v -> Just v
                                    Nothing -> lookupEnv i parent

pushFreshEnv :: Environment -> Environment
pushFreshEnv = Env M.empty

popEnv :: Environment -> Environment
popEnv (Env _ p) = p

withFreshEnv :: EvalM a -> EvalM ()
withFreshEnv f = modifyEnv (pushFreshEnv) >> f >> modifyEnv (popEnv)

modifyEnv :: (Environment -> Environment) -> EvalM ()
modifyEnv f = get >>= (put . f)
