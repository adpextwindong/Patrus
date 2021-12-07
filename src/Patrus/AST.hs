{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.AST where

import Debug.Trace
import GHC.Float
import Prelude hiding (EQ,LT,GT)

type Program = [Statement]
type Identifier = String

data Statement = ExprStatement Expr
               | PrintStatement Expr
               | VarDeclaration Identifier (Maybe Expr)
               deriving Show

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
          | Var Identifier
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
eval e@(Lit _) = return e
eval (Group e) = eval e
eval (UOp Negate e) = case eval e of
                        Right (Lit (NumberLit x)) -> Right $ Lit $ NumberLit (-x)
                        _ -> Left $ UOPTyMismatch "Operand must be a number."
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
literalBopTyMatch :: BinOp -> Expr -> Expr -> Either Error (Expr,Expr)
literalBopTyMatch operator e1 e2 = do
    e1 <- eval e1
    e2 <- eval e2
    if not $ sameLitType e1 e2
    then case operator of
            Plus -> Left plusTyMismatch
            _    -> Left bopTyMismatch
    else
        return (e1,e2)

sameLitType :: Expr -> Expr -> Bool
sameLitType (Lit (NumberLit _)) (Lit (NumberLit _)) = True
sameLitType (Lit (StringLit _)) (Lit (StringLit _)) = True
sameLitType (Lit (BoolLit _)) (Lit (BoolLit _)) = True
sameLitType _ _ = False

evalTruthy :: ComparrisonOp -> Expr -> Expr -> Either Error Expr
evalTruthy operator e1 e2 = do
   e1 <- eval e1
   e2 <- eval e2
   return $ case operator of
        EQ -> Lit $ BoolLit $ literalTruth e1 == literalTruth e2
        NEQ -> Lit $ BoolLit $ literalTruth e1 /= literalTruth e2

evalNotTruthy :: Expr -> Either Error Expr
evalNotTruthy e = case eval e of
                    Left err -> Left err --A subexpression failed to evaluate
                    Right (Lit (BoolLit b)) -> Right $ Lit $ BoolLit (not b)    --Bools are bools
                    Right (Lit Nil) -> Right $ Lit $ BoolLit True               --Nil is falsy
                    Right e -> Right $ Lit $ BoolLit False                      --Every else is truthy

literalTruth :: Expr -> Bool
literalTruth (Lit Nil) = False
literalTruth (Lit (BoolLit False)) = False
literalTruth _ = True

interpret :: Program -> IO ()
interpret ((PrintStatement e):xs) = do
    case eval e of
        Left e' -> putStrLn $ "INTERPRET " <> show e' --TODO pretty print
        Right val -> putStrLn (show val) >> interpret xs
interpret ((ExprStatement e):xs) = do
    --TODO port eval to some state containing monad
    interpret xs
interpret [] = return ()
