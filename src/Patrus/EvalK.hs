{-# LANGUAGE LambdaCase #-}
module Patrus.EvalK where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace ( trace )
import Data.Time.Clock (diffTimeToPicoseconds, getCurrentTime, UTCTime(utctDayTime))

import Patrus.Environment
import Patrus.Types as P
import Patrus.Eval.Pure

uopTyMismatch = "Operand must be a number."
bopTyMismatch = "Operands must be numbers."
plusTyMismatch = "Operands must be two numbers or two strings."

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

evalK (Group e) env k = evalK e env k

evalK (UOp Negate e) env k = evalK e env (\e' env' -> case e' of
                                            (Lit (NumberLit x)) -> k (Lit (NumberLit (-x))) env'
                                            _ -> fail uopTyMismatch)

--Dispatch to truthy evals
evalK (UOp Not e) env k = evalK e env (\e' -> k (notTruthy e'))

evalK (BOp (Cmp P.EQ) e1 e2) env k = evalK e1 env
  (\e1' env' -> evalK e2 env' (\e2' env'' -> k (truthy P.EQ e1' e2') env'') env')

evalK (BOp (Cmp P.NEQ) e1 e2) env k = evalK e1 env
  (\e1' env' -> evalK e2 env' (\e2' env'' -> k (truthy P.NEQ e1' e2') env'') env')
{-
evalK (BOp And e1 e2) env k = evalKTruthyShortCircuit And e1 e2 env k
evalK (BOp Or e1 e2) env k = evalKTruthyShortCircuit Or e1 e2 env k
-}

evalK (Call callee args) env k = undefined --TODO add caller continuation to Environment

evalK _ _ _ = undefined

--Boolean not in the Truthyness of Lox
notTruthy :: Expr -> Expr
notTruthy = \case
  (Lit (BoolLit b)) -> Lit $ BoolLit (not b) --Bools are bools
  (Lit Nil) -> Lit $ BoolLit True            --Nil is falsy
  e -> Lit $ BoolLit False                   --Everything else is truthy

truthy :: ComparrisonOp -> Expr -> Expr -> Expr
truthy operator e1 e2 = case operator of
                          P.EQ -> Lit $ BoolLit $ literalTruth e1 == literalTruth e2
                          NEQ -> Lit $ BoolLit $ literalTruth e1 /= literalTruth e2

evalKTruthyShortCircuit :: BinOp -> Expr -> Expr -> Environment -> (Store -> IO Store) -> (Store -> IO Store)
evalKTruthyShortCircuit = undefined --TODO


interpretK :: Program -> Environment -> (Store -> IO Store) -> (Store -> IO Store)
interpretK [] _ k = k
interpretK ((PrintStatement e) : xs) env k = evalK e env (\e' env' -> do
  print ("PRINT: " <> show e)
  interpretK xs env k env)


tprintAdd = interpretK [(PrintStatement (BOp Plus (Lit (NumberLit 1.0)) (Lit (NumberLit 2.0))))] EmptyEnv return EmptyEnv
tprint = interpretK [(PrintStatement (Lit (NumberLit 42.0)))] EmptyEnv return EmptyEnv
