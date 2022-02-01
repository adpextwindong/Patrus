module EvalK where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace ( trace )

import Patrus.Environment
import Patrus.Types


errVarError identifier = error $ "Undefined variable '" <> identifier <> "'."

type Store = Environment --Unify store/env??
type Cont = Store -> Store
type K = Expr -> Cont

--Used for debugging evalK directly
kTrace :: K
kTrace e = trace (show e) id

--Monadic Expression Continuation (for printing/etc...)
type KM m = Expr -> Store -> m Store

evalK :: Expr -> Environment -> KM IO -> Store -> IO Store
evalK e@(Lit _) env k = k e
evalK (Var i) env k = do
    case lookupEnv i env of
        Nothing -> errVarError i
        Just v -> k v
evalK _ _ _ = undefined

interpretK :: Statement -> Environment -> (Store -> IO Store) -> (Store -> IO Store)
interpretK = undefined