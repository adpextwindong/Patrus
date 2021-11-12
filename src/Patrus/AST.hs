module Patrus.AST where

import GHC.Float
import Prelude hiding (EQ,LT,GT)

data BinOp = EQ | NEQ | LT | LTE | GT | GTE
           | Plus | Minus | Mul | Div

data UnaryOp = Negate | Not

data Literal = NumberLit Double
             | StringLit String
             | TrueLit
             | FalseLit
             | Nil

data Expr = BOp BinOp Expr Expr
          | UOp UnaryOp Expr
          | Lit Literal
          | Group Expr

wrapParens :: String -> String
wrapParens s = "(" ++ s ++ ")"

prettyPrintAST :: Expr -> String
prettyPrintAST (BOp op e1 e2)   = wrapParens $ prettyPrintBop op ++ " " ++ prettyPrintAST e1 ++ " " ++ prettyPrintAST e2
prettyPrintAST (UOp op e)       = wrapParens $ prettyPrintUop op ++ prettyPrintAST e
prettyPrintAST (Group e)        = wrapParens $ "group " ++ prettyPrintAST e
prettyPrintAST (Lit v)          = prettyPrintLit v

prettyPrintLit :: Literal -> String
prettyPrintLit (NumberLit v)    = formatFloat v
prettyPrintLit (StringLit s)    = show s
prettyPrintLit (TrueLit)        = show True
prettyPrintLit (FalseLit)       = show False
prettyPrintLit (Nil)            = "nil"

prettyPrintBop :: BinOp -> String
prettyPrintBop (EQ)     = "=="
prettyPrintBop (NEQ)    = "!="
prettyPrintBop (LT)     = "<"
prettyPrintBop (LTE)    = "<="
prettyPrintBop (GT)     = ">"
prettyPrintBop (GTE)    = ">="
prettyPrintBop (Plus)   = "+"
prettyPrintBop (Minus)  = "-"
prettyPrintBop (Mul)    = "*"
prettyPrintBop (Div)    = "/"

prettyPrintUop :: UnaryOp -> String
prettyPrintUop (Negate) = "-"
prettyPrintUop (Not)    = "!"

--jlox formats Doubles different than Haskell's show.
--https://stackoverflow.com/a/35980995
formatFloat :: RealFloat a => a -> String
formatFloat v
  | v == 0                          = "0"
  | abs v < 1e-5 || abs v > 1e10    = formatRealFloat FFExponent Nothing v
  | v - fromIntegral (floor v) == 0 = formatRealFloat FFFixed   (Just 0) v
  | otherwise                       = formatRealFloat FFGeneric  Nothing v