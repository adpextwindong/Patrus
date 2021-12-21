{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.Eval where

import Debug.Trace
import GHC.Float
import Prelude hiding (EQ,LT,GT)

import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))

import Patrus.Environment
import Patrus.Types

uopTyMismatch = "Operand must be a number."
bopTyMismatch = "Operands must be numbers."
plusTyMismatch = "Operands must be two numbers or two strings."

runtimeVarError :: Identifier -> String
runtimeVarError identifier = "Undefined variable '" <> identifier <> "'."

-- Evaluate the expression or return an error.
-- All trivial patterns where we can perform the operator are done here.
-- Type checking and subexpression handling is done elsewhere.
eval :: Expr -> EvalM Expr
eval e@(Lit _) = return e
eval (Var i) = do
    env <- get
    case lookupEnv i env of
        Nothing -> fail $ runtimeVarError i
        Just v -> return v

eval (Assignment i e) = do
   e' <- eval e
   modifyEnv $ insertEnv i e'
   return e'

eval (Group e) = eval e
eval (UOp Negate e) = do
    e' <- eval e
    case e' of
        (Lit (NumberLit x)) -> pure $ Lit $ NumberLit (-x)
        _ -> fail uopTyMismatch

--Dispatch to truthy evals
eval (UOp Not e) = evalNotTruthy e
eval (BOp (Cmp EQ) e1 e2) = evalTruthy EQ e1 e2
eval (BOp (Cmp NEQ) e1 e2) = evalTruthy NEQ e1 e2

--Strict Type Matching operators
-- Sub expressions need to be evaluated and type mismatches should be handled.
eval (BOp operator e1 e2) = do
    (e1,e2) <- literalBopTyMatch operator e1 e2
    return $ evalBop operator e1 e2

evalBop :: BinOp -> Expr -> Expr -> Expr
evalBop (Cmp LT)  (Lit (BoolLit a)) (Lit (BoolLit b)) = Lit $ BoolLit $ a < b
evalBop (Cmp LTE) (Lit (BoolLit a)) (Lit (BoolLit b)) = Lit $ BoolLit $ a <= b
evalBop (Cmp GT)  (Lit (BoolLit a)) (Lit (BoolLit b)) = Lit $ BoolLit $ a > b
evalBop (Cmp GTE) (Lit (BoolLit a)) (Lit (BoolLit b)) = Lit $ BoolLit $ a >= b

evalBop Plus (Lit (StringLit s1)) (Lit (StringLit s2)) = Lit $ StringLit (s1 <> s2)
evalBop Plus (Lit (NumberLit x)) (Lit (NumberLit y))   = Lit $ NumberLit (x + y)
evalBop Minus (Lit (NumberLit x)) (Lit (NumberLit y))  = Lit $ NumberLit (x - y)
evalBop Mul (Lit (NumberLit x)) (Lit (NumberLit y))    = Lit $ NumberLit (x * y)
evalBop Div (Lit (NumberLit x)) (Lit (NumberLit y))    = Lit $ NumberLit (x / y)

--evalBop op e1 e2 = trace ("EXHAUST: "<> show op <> " " <> show e1 <> " " <> show e2) $ undefined

-- | Performs strict type matching for PLUS/MINUS/DIV/MUL/LT/LTE/GT/GTE
-- Nil in any operand causes an error, EQ and NEQ should be handled earlier.
literalBopTyMatch :: BinOp -> Expr -> Expr -> EvalM (Expr,Expr)
literalBopTyMatch operator e1 e2 = do
    e1 <- eval e1
    e2 <- eval e2
    if not $ sameLitType e1 e2
    then case operator of
            Plus -> fail plusTyMismatch
            _    -> fail bopTyMismatch
    else
        pure (e1,e2)

sameLitType :: Expr -> Expr -> Bool
sameLitType (Lit (NumberLit _)) (Lit (NumberLit _)) = True
sameLitType (Lit (StringLit _)) (Lit (StringLit _)) = True
sameLitType (Lit (BoolLit _)) (Lit (BoolLit _)) = True
sameLitType _ _ = False

evalTruthy :: ComparrisonOp -> Expr -> Expr -> EvalM Expr
evalTruthy operator e1 e2 = do
   e1 <- eval e1
   e2 <- eval e2
   return $ case operator of
        EQ -> Lit $ BoolLit $ literalTruth e1 == literalTruth e2
        NEQ -> Lit $ BoolLit $ literalTruth e1 /= literalTruth e2

evalNotTruthy :: Expr -> EvalM Expr
evalNotTruthy e = do
    e' <- eval e
    return $ case e' of
        (Lit (BoolLit b)) -> Lit $ BoolLit (not b)    --Bools are bools
        (Lit Nil) -> Lit $ BoolLit True               --Nil is falsy
        e -> Lit $ BoolLit False                      --Every else is truthy

literalTruth :: Expr -> Bool
literalTruth (Lit Nil) = False
literalTruth (Lit (BoolLit False)) = False
literalTruth _ = True
