module Patrus.Eval.PrettyPrinter
    ( prettyPrintAST ) where

import GHC.Float
import Prelude as P

import Patrus.Types as A

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
prettyPrintLit (BoolLit b)      = show b
prettyPrintLit (Nil)            = "nil"

prettyPrintBop :: BinOp -> String
prettyPrintBop (Cmp (A.EQ))  = "=="
prettyPrintBop (Cmp (NEQ))   = "!="
prettyPrintBop (Cmp (A.LT))  = "<"
prettyPrintBop (Cmp (LTE))   = "<="
prettyPrintBop (Cmp (A.GT))  = ">"
prettyPrintBop (Cmp (A.GTE)) = ">="
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
