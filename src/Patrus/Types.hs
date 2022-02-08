{-# LANGUAGE DerivingStrategies #-}         --EvalM
{-# LANGUAGE GeneralizedNewtypeDeriving #-} --EvalM
module Patrus.Types where

import Control.Monad.Except
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
          | Func Function Env
          | Call Expr [Expr]
                    | NativeFunc {
                nativeFuncTag :: NativeTag
               ,callparameters :: [Identifier]
          }
          | Class --TODO
            deriving (Show, Eq)

data Function = Function {
                fnparameters :: [Identifier]
               ,body :: Statement
          } deriving (Show, Eq)

--IO() can't have an EQ instance so dispatching on this tag will be less of a hassle for now
--Tag for native function
data NativeTag = Clock
    deriving (Show, Eq)

--Datatype for handling identifier bindings
data Env = Env {
            scope :: M.Map Identifier Expr
           ,enclosing :: Env
          } | EmptyEnv
          deriving (Show, Eq)

--Datatype for Runtime Environment
--Any continuations for control flow will go here
data Environment = Environment {
                      env :: Env
                     ,global :: Env
                     ,fnReturnK :: Maybe (Expr -> Environment -> IO Environment, Environment) --TODO maybe or list??
                      --Maybe tag this with source location for printing
                   }

setGlobal :: Environment -> Env -> Environment
setGlobal (Environment e _ fk) global = Environment e global fk

instance Show Environment where
  show (Environment env global Nothing) = "Environment " <> show env <> " Global "<> show global <> " no returnK"
  show (Environment env global (Just xs)) = "Environment " <> show env <> " Global" <> show global <> " has returnK"

data EvalException = ReturnException Expr
                   | EvalPanic String --Error String
                   | EndOfBlock
                   deriving (Show, Eq)

--like Intrigue's EvalM but with StateT
newtype EvalM a = EvalM { runEval :: ExceptT EvalException (StateT Environment IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState Environment
                   , MonadFail --Evaluation can fail due to type mismatches (or soon undeclared identifiers)
                   , MonadIO
                   , MonadError EvalException
                   )

desugarFor :: Maybe Statement -> Maybe Expr -> Maybe Expr -> Statement -> Statement
desugarFor initializer condition increment body = BlockStatement $ forInit ++ [forBody]
  where
    forInit = maybeToList initializer
    forCondition = fromMaybe (Lit (BoolLit True)) condition
    forBody = WhileStatement forCondition $ BlockStatement $ body : maybeToList (fmap ExprStatement increment)
