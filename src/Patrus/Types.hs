{-# LANGUAGE DerivingStrategies #-}         --EvalM
{-# LANGUAGE GeneralizedNewtypeDeriving #-} --EvalM
module Patrus.Types where

import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, execStateT, modify')
import qualified Data.Map.Strict as M
import Data.Maybe

type Program = [Statement]
type Identifier = String

data Statement = ExprStatement Expr
               | PrintStatement Expr
               | VarDeclaration Identifier (Maybe Expr)
               | FunStatement Identifier [Identifier] Statement
               | BlockStatement [Statement]
               | IfStatement Expr Statement (Maybe Statement)
               | WhileStatement Expr Statement
               | DumpStatement
               | ReturnStatement (Maybe Expr)
               deriving (Show, Eq)

data ComparrisonOp = EQ | NEQ | LT | LTE | GT | GTE
    deriving (Show, Eq)

data BinOp = Cmp ComparrisonOp
           | Plus | Minus | Mul | Div
           | Or | And
           deriving (Show, Eq)

data UnaryOp = Negate | Not
    deriving (Show, Eq)

data Literal = NumberLit Double
             | StringLit String
             | BoolLit Bool
             | Nil
                deriving (Show, Eq)

data Expr = BOp BinOp Expr Expr
          | UOp UnaryOp Expr
          | Lit Literal
          | Group Expr
          | Var Identifier
          | Assignment Identifier Expr
          | Func Function Environment
          | Call Expr [Expr]
                    | NativeFunc {
                nativeFuncTag :: NativeTag
               ,callparameters :: [Identifier]
          }
          | Class --TODO
          | Unit --TODO remove after we figure out return statements
            deriving (Show, Eq)

data Function = Function {
                fnparameters :: [Identifier]
               ,body :: Statement
          } deriving (Show, Eq)

--IO() can't have an EQ instance so dispatching on this tag will be less of a hassle for now
--Tag for native function
data NativeTag = Clock
    deriving (Show, Eq)

data Environment = Env {
                       scope :: M.Map Identifier Expr
                      ,enclosing :: Environment
                   } | EmptyEnv
                    deriving (Show, Eq)

--like Intrigue's EvalM but with StateT
newtype EvalM a = EvalM { runEval :: StateT Environment IO a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState Environment
                   , MonadFail --Evaluation can fail due to type mismatches (or soon undeclared identifiers)
                   , MonadIO
                   )

desugarFor :: Maybe Statement -> Maybe Expr -> Maybe Expr -> Statement -> Statement
desugarFor initializer condition increment body = BlockStatement $ forInit ++ [forBody]
  where
    forInit = maybeToList initializer
    forCondition = fromMaybe (Lit (BoolLit True)) condition
    forBody = WhileStatement forCondition $ BlockStatement $ body : maybeToList (fmap ExprStatement increment)
