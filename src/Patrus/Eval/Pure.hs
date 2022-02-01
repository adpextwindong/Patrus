module Patrus.Eval.Pure where
import Patrus.Types as P

--literalBopTyMatch should catch before this
-- Handles all nonTruthy operators
evalBop :: BinOp -> Expr -> Expr -> Expr
evalBop (Cmp P.LT)  (Lit (NumberLit a)) (Lit (NumberLit b)) = Lit $ BoolLit $ a < b
evalBop (Cmp LTE) (Lit (NumberLit a)) (Lit (NumberLit b))   = Lit $ BoolLit $ a <= b
evalBop (Cmp P.GT)  (Lit (NumberLit a)) (Lit (NumberLit b)) = Lit $ BoolLit $ a > b
evalBop (Cmp GTE) (Lit (NumberLit a)) (Lit (NumberLit b))   = Lit $ BoolLit $ a >= b

evalBop Plus (Lit (StringLit s1)) (Lit (StringLit s2)) = Lit $ StringLit (s1 <> s2)
evalBop Plus (Lit (NumberLit x)) (Lit (NumberLit y))   = Lit $ NumberLit (x + y)
evalBop Minus (Lit (NumberLit x)) (Lit (NumberLit y))  = Lit $ NumberLit (x - y)
evalBop Mul (Lit (NumberLit x)) (Lit (NumberLit y))    = Lit $ NumberLit (x * y)
evalBop Div (Lit (NumberLit x)) (Lit (NumberLit y))    = Lit $ NumberLit (x / y)

evalBop _ _ _ = error "Operands must be numbers (or two strings for append)."

sameLitType :: Expr -> Expr -> Bool
sameLitType (Lit (NumberLit _)) (Lit (NumberLit _)) = True
sameLitType (Lit (StringLit _)) (Lit (StringLit _)) = True
sameLitType (Lit (BoolLit _)) (Lit (BoolLit _)) = True
sameLitType _ _ = False

literalTruth :: Expr -> Bool
literalTruth (Lit Nil) = False
literalTruth (Lit (BoolLit False)) = False
literalTruth _ = True

