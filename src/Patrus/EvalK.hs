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

evalK :: Expr -> KM IO -> (Store -> IO Store)
evalK e@(Lit _) k env = k e env

evalK (Var i) k env =
    case lookupEnv i env of
        Nothing -> errVarError i
        Just v -> k v env

evalK (Assignment i e) k  env = evalK e (\e' env' -> adjustEnvFM i e' env' >>= k e') env

evalK (NativeFunc Clock []) k env = do
  picoseconds <- fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
  let seconds = Lit $ NumberLit $ picoseconds / 10^^12
  k seconds env

evalK (Group e) k env = evalK e k env

evalK (UOp Negate e) k env = evalK e (\e' env' -> case e' of
                                            (Lit (NumberLit x)) -> k (Lit (NumberLit (-x))) env'
                                            _ -> fail uopTyMismatch) env

--Dispatch to truthy evals
evalK (UOp Not e) k env = evalK e (k . notTruthy) env

evalK (BOp (Cmp P.EQ) e1 e2) k env = evalK e1
  (\e1' env' -> evalK e2 (k . truthy P.EQ e1') env') env

evalK (BOp (Cmp P.NEQ) e1 e2) k env = evalK e1
  (\e1' env' -> evalK e2 (k . truthy P.NEQ e1') env') env

evalK (BOp And e1 e2) k env = undefined --TODO
evalK (BOp Or e1 e2) k env = undefined --TODO

evalK (Call callee args) env k = undefined --TODO add caller continuation to Environment
evalK _ _ _ = undefined

evalKTruthyShortCircuit :: BinOp -> Expr -> Expr -> (Store -> IO Store) -> (Store -> IO Store)
evalKTruthyShortCircuit = undefined --TODO

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

interpretK :: Program -> (Store -> IO Store) -> (Store -> IO Store)
interpretK [] k = k

interpretK ((PrintStatement e) : xs) k = evalK e (\e' env' -> do
  print ("PRINT: " <> show e)
  interpretK xs k env')



tprintAdd = interpretK [PrintStatement (BOp Plus (Lit (NumberLit 1.0)) (Lit (NumberLit 2.0)))] return EmptyEnv
tptrint = interpretK [PrintStatement (Lit (NumberLit 42.0))] return EmptyEnv
