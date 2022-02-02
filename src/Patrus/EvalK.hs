module Patrus.EvalK where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace ( trace )
import Data.Time.Clock (diffTimeToPicoseconds, getCurrentTime, UTCTime(utctDayTime))

import Patrus.Environment
import Patrus.Types

errVarError identifier = error $ "Undefined variable '" <> identifier <> "'."

type Store = Environment --Unify store/env??
type Cont = Store -> Store

--Used for debugging evalK directly
kTraceM :: KM IO
kTraceM e = trace (show e) return

--Monadic Expression Continuation (for printing/etc...)
type KM m = Expr -> Store -> m Store

evalK :: Expr -> Environment -> KM IO -> Store -> IO Store
evalK e@(Lit _) env k = k e

evalK (Var i) env k = do
    case lookupEnv i env of
        Nothing -> errVarError i
        Just v -> k v

evalK (Assignment i e) env k = evalK e env (\e' env' -> adjustEnvFM i e' env' >>= k e')

evalK (NativeFunc Clock []) env k = \s -> do
  picoseconds <- fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
  let seconds = Lit $ NumberLit $ picoseconds / 10^^12
  k seconds s

evalK _ _ _ = undefined

interpretK :: Program -> Environment -> (Store -> IO Store) -> (Store -> IO Store)
interpretK [] _ k = k
interpretK ((PrintStatement e) : xs) env k = evalK e env (\e' env' -> do
  print ("PRINT: " <> show e)
  interpretK xs env k env)


tprintAdd = interpretK [(PrintStatement (BOp Plus (Lit (NumberLit 1.0)) (Lit (NumberLit 2.0))))] EmptyEnv return EmptyEnv
tprint = interpretK [(PrintStatement (Lit (NumberLit 42.0)))] EmptyEnv return EmptyEnv
