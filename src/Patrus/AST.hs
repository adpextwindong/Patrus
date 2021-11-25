module Patrus.AST where

import Debug.Trace
import GHC.Float
import Prelude hiding (EQ,LT,GT)

data ComparrisonOp = EQ | NEQ | LT | LTE | GT | GTE
    deriving Show

data BinOp = Cmp ComparrisonOp
           | Plus | Minus | Mul | Div
           deriving Show

data UnaryOp = Negate | Not
    deriving Show

data Literal = NumberLit Double
             | StringLit String
             | BoolLit Bool
             | Nil
                deriving Show

data Expr = BOp BinOp Expr Expr
          | UOp UnaryOp Expr
          | Lit Literal
          | Group Expr
            deriving Show

data Error = BOPTyMismatch String
           | UOPTyMismatch String
           deriving Show

bopTyMismatch = BOPTyMismatch "Operands must be numbers."
plusTyMismatch = BOPTyMismatch "Operands must be two numbers or two strings."

-- Evaluate the expression or return an error.
-- All trivial patterns where we can perform the operator are done here.
-- Type checking and subexpression handling is done elsewhere.
eval :: Expr -> Either Error Expr
eval e@(Lit _) = Right e
eval (Group e) = eval e
eval (UOp Negate e) = case eval e of
                        Right (Lit (NumberLit x)) -> Right $ Lit $ NumberLit (-x)
                        _ -> Left $ UOPTyMismatch "Operand must be a number."
--Dispatch to truthy evals
eval (UOp Not e) = evalNotTruthy e
eval (BOp (Cmp EQ) e1 e2) = evalTruthy EQ (eval e1) (eval e2)
eval (BOp (Cmp NEQ) e1 e2) = evalTruthy NEQ (eval e1) (eval e2)
--Strict Type Matching operators
eval (BOp (Cmp LT)  (Lit (BoolLit a)) (Lit (BoolLit b))) = Right $ Lit $ BoolLit $ a < b
eval (BOp (Cmp LTE) (Lit (BoolLit a)) (Lit (BoolLit b))) = Right $ Lit $ BoolLit $ a <= b
eval (BOp (Cmp GT)  (Lit (BoolLit a)) (Lit (BoolLit b))) = Right $ Lit $ BoolLit $ a > b
eval (BOp (Cmp GTE) (Lit (BoolLit a)) (Lit (BoolLit b))) = Right $ Lit $ BoolLit $ a >= b

eval (BOp Plus (Lit (StringLit s1)) (Lit (StringLit s2))) = Right $ Lit $ StringLit (s1 <> s2)
eval (BOp Plus (Lit (NumberLit x)) (Lit (NumberLit y)))   = Right $ Lit $ NumberLit (x + y)
eval (BOp Minus (Lit (NumberLit x)) (Lit (NumberLit y)))  = Right $ Lit $ NumberLit (x - y)
eval (BOp Mul (Lit (NumberLit x)) (Lit (NumberLit y)))    = Right $ Lit $ NumberLit (x * y)
eval (BOp Div (Lit (NumberLit x)) (Lit (NumberLit y)))    = Right $ Lit $ NumberLit (x / y)

-- Sub expressions need to be evaluated and type mismatches should be handled.
eval (BOp operator e1 e2) = case literalBopTyMatch (eval e1) (eval e2) of
                                Left _ -> case operator of
                                            Plus -> Left plusTyMismatch
                                            _ ->    Left bopTyMismatch

                                Right (e1',e2') -> eval (BOp operator e1' e2')

--eval e = trace ("EXHAUST: "<> show e) $ Left undefined

-- | Performs strict type matching for PLUS/MINUS/DIV/MUL/LT/LTE/GT/GTE
-- Nil in any operand causes an error, EQ and NEQ should be handled earlier.
literalBopTyMatch :: Either Error Expr -> Either Error Expr -> Either Error (Expr,Expr)
literalBopTyMatch (Left err) _ = Left err
literalBopTyMatch _ (Left err) = Left err
literalBopTyMatch (Right e1@(Lit (NumberLit _))) (Right e2@(Lit (NumberLit _))) = Right (e1,e2)
literalBopTyMatch (Right e1@(Lit (StringLit _))) (Right e2@(Lit (StringLit _))) = Right (e1,e2)
literalBopTyMatch (Right e1@(Lit (BoolLit _ ))) (Right e2@(Lit (BoolLit _ ))) = Right (e1, e2)
literalBopTyMatch _ _ = Left bopTyMismatch

evalTruthy :: ComparrisonOp -> Either Error Expr -> Either Error Expr -> Either Error Expr
evalTruthy _ err@(Left _) _ = err
evalTruthy _ _ err@(Left _) = err
evalTruthy EQ (Right (Lit a)) (Right (Lit b)) = Right $ Lit $ BoolLit $ literalTruth a == literalTruth b
evalTruthy NEQ (Right (Lit a)) (Right (Lit b)) = Right $ Lit $ BoolLit $ literalTruth a /= literalTruth b

evalNotTruthy :: Expr -> Either Error Expr
evalNotTruthy e = case eval e of
                    Left err -> Left err --A subexpression failed to evaluate
                    Right (Lit (BoolLit b)) -> Right $ Lit $ BoolLit (not b)    --Bools are bools
                    Right (Lit Nil) -> Right $ Lit $ BoolLit True               --Nil is falsy
                    Right e -> Right $ Lit $ BoolLit False                      --Every else is truthy

literalTruth :: Literal -> Bool
literalTruth Nil = False
literalTruth (BoolLit False) = False
literalTruth _ = True
