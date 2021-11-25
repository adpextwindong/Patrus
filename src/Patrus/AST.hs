module Patrus.AST where

import Debug.Trace
import GHC.Float
import Prelude hiding (EQ,LT,GT)

data BinOp = EQ | NEQ | LT | LTE | GT | GTE
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

bopTyMismatch = BOPTyMismatch "Operands have mismatching types."
plusTyMismatch = BOPTyMismatch "Operands must be two numbers or two strings."

eval :: Expr -> Either Error Expr
eval e@(Lit _) = Right e
eval (Group e) = eval e
eval (BOp Plus (Lit (StringLit s1)) (Lit (StringLit s2))) = Right $ Lit $ StringLit (s1 <> s2)
eval (BOp Plus (Lit (NumberLit x)) (Lit (NumberLit y)))   = Right $ Lit $ NumberLit (x + y)

eval (BOp Plus e1 e2) = case literalTyMatch (eval e1) (eval e2) of
                            Left _ -> Left plusTyMismatch
                            Right (e1', e2') -> eval (BOp Plus e1' e2')

eval e = trace ("EXHAUST: "<> show e) $ Left undefined

-- | Performs strict type matching
literalTyMatch :: Either Error Expr -> Either Error Expr -> Either Error (Expr,Expr)
literalTyMatch (Left err) _ = Left err
literalTyMatch _ (Left err) = Left err
literalTyMatch (Right e1@(Lit (StringLit _))) (Right e2@(Lit (StringLit _))) = Right (e1,e2)
literalTyMatch (Right e1@(Lit (NumberLit _))) (Right e2@(Lit (NumberLit _))) = Right (e1,e2)
literalTyMatch (Right e1@(Lit (BoolLit _ ))) (Right e2@(Lit (BoolLit _ ))) = Right (e1, e2)
literalTyMatch _ _ = Left bopTyMismatch
