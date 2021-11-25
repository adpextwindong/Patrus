module Patrus.AST where

import GHC.Float
import Prelude hiding (EQ,LT,GT)

data BinOp = EQ | NEQ | LT | LTE | GT | GTE
           | Plus | Minus | Mul | Div
           deriving Show

data UnaryOp = Negate | Not
    deriving Show

data Literal = NumberLit Double
             | StringLit String
             | TrueLit
             | FalseLit
             | Nil
                deriving Show

data Expr = BOp BinOp Expr Expr
          | UOp UnaryOp Expr
          | Lit Literal
          | Group Expr
            deriving Show
