{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
module Patrus.Eval where

import Debug.Trace
import GHC.Float
import Prelude hiding (EQ,LT,GT)
import Data.Time.Clock

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))

import Patrus.Env
import Patrus.Types
import Patrus.Eval.Pure (evalBop, sameLitType, literalTruth, equality, notTruthy)
import Patrus.Eval.PrettyPrinter (prettyPrintAST)
import Patrus.Env (insertEnvironment, withFuncEnvironment)

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
    (Environment env global _) <- get
    case lookupEnv i env of
        Nothing -> case lookupEnv i global of
                     Nothing -> fail $ runtimeVarError i
                     Just v -> return v
        Just v -> return v

eval (Assignment i e) = do
   e' <- eval e
   put =<< adjustEnvironmentFM i e' =<< get
   return e'

eval (NativeFunc Clock []) = do
    picoseconds <- liftIO $ fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
    let seconds = picoseconds / 10^^12
    return $ Lit $ NumberLit seconds

eval (NativeFunc Clock xs) = fail $ "Expected " <> show 0 <> " arguments but got " <> show (length xs) <> "."


eval (Call callee args) = do
    callee' <- eval callee
    args' <- mapM eval args
    case callee' of
        e@(NativeFunc _ _ ) -> eval e         --Now that callee' is fully evaluated we can pattern match again
        e@(Func name (Function params body) closure) -> do
            callTyCheck callee'
            arityCheck params args'
            let bindings = (name, e) : zip params args' --Allow for recursion but bind before params
            --Stash callee environment for restoring
            oldEnv <- get
            --Setup closure environment
            put $ Environment closure (global oldEnv) Nothing
            do { withFuncEnvironment bindings $ interpretM [body] }
              `catchError`
              (\case
                (ReturnException e) -> do
                    (Environment _ globals' _) <- get
                    --Thread through modified global variables to the original callee
                    --restore callee environment
                    put $ setGlobal oldEnv globals'
                    return e
                err -> throwError err)
        _ -> fail $ "Type Error: can't call a non function"

eval (Group e) = eval e
eval (UOp Negate e) = do
    e' <- eval e
    case e' of
        (Lit (NumberLit x)) -> pure $ Lit $ NumberLit (-x)
        other -> pure $ Lit $ BoolLit $ not . literalTruth $ other

--Dispatch to truthy evals
eval (UOp Not e) = eval e >>= (return . notTruthy)
eval (BOp (Cmp EQ) e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  return $ equality EQ e1' e2'

eval (BOp (Cmp NEQ) e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  return $ equality NEQ e1' e2'

eval (BOp And e1 e2) = evalTruthyShortCircuit And e1 e2
eval (BOp Or e1 e2) = evalTruthyShortCircuit Or e1 e2

--Strict Type Matching operators
-- Sub expressions need to be evaluated and type mismatches should be handled.
eval (BOp operator e1 e2) = do
    (e1,e2) <- literalBopTyMatch operator e1 e2
    pure $ evalBop operator e1 e2

eval e@(Func _ _ _) = return e
eval Class = undefined --TODO

-- | Performs strict type matching for PLUS/MINUS/DIV/MUL/LT/LTE/GT/GTE
-- Nil in any operand causes an error, EQ and NEQ should be handled earlier.
literalBopTyMatch :: BinOp -> Expr -> Expr -> EvalM (Expr,Expr)
literalBopTyMatch operator e1 e2 = do
    e1 <- eval e1
    e2 <- eval e2
    if not $ sameLitType e1 e2
    then case operator of
            Plus -> do
                liftIO $ print $ "ATTEMPTED: " <> show operator <> " " <> show e1 <> " " <> show e2
                fail plusTyMismatch
            _    -> fail bopTyMismatch
    else
        pure (e1,e2)

callTyCheck :: Expr -> EvalM Expr
callTyCheck e@(Func _ _ _) = return e
callTyCheck e@(Class) = return e
callTyCheck e = fail $ "Can only call functions and classes."

arityCheck :: [Identifier] -> [Expr] -> EvalM ()
arityCheck params args = if length params == length args
                         then return ()
                         else fail $ "Expected " <> show (length params) <> " arguments but got " <> show (length args) <> "."


evalTruthyShortCircuit :: BinOp -> Expr -> Expr -> EvalM Expr
evalTruthyShortCircuit And e1 e2 = do
    e1' <- eval e1
    if not . literalTruth $ e1'
    then return e1'
    else eval e2

evalTruthyShortCircuit Or e1 e2 = do
    e1' <- eval e1
    if literalTruth e1'
    then return e1'
    else eval e2

interpretM :: Program -> EvalM Expr
--interpretM ps | trace ("\nTRICK " <> show ps) False = undefined

interpretM [] = throwError EndOfBlock

interpretM ((PrintStatement e): xs) = do
    e' <- eval e
    liftIO $ putStrLn $ prettyPrintAST e'
    interpretM xs

interpretM (DumpStatement : xs) = do
    env <- get
    liftIO $ print $ "DUMP: " <> show env
    interpretM xs

interpretM ((ExprStatement e) : xs) = do
    _ <- eval e
    interpretM xs

interpretM ((VarDeclaration i Nothing) : xs) = do
    modifyEnvironment $ insertEnvironment i (Lit Nil)
    interpretM xs

interpretM (VarDeclaration i (Just e) : xs) = do
    e' <- eval e
    modifyEnvironment (insertEnvironment i e')
    interpretM xs

interpretM ((BlockStatement bs):xs) = do
    withFuncEnvironment [] (interpretM bs) `catchError` (\case
      EndOfBlock -> interpretM xs
      e -> throwError e)

--1/7/22 TODO this is begging us to have an interpretBlock function
interpretM ((IfStatement conde trueBranch falseBranch): xs) = do
    e <- eval conde

    blockVal <- if literalTruth e
                then interpretM [trueBranch]
                else case falseBranch of
                    Just fb -> interpretM [fb]
                    Nothing -> interpretM []

    interpretM xs
    --TODO refactor EvalM to handle this better

interpretM w@((WhileStatement conde body):xs) = do
    e <- eval conde

    --TODO test for similar Unit handling. Any block interpretting code needs to deal with returns...
    if literalTruth e
    then interpretM [body] >> interpretM w
    else interpretM xs

interpretM ((ReturnStatement Nothing): xs) = throwError $ ReturnException $ Lit Nil
interpretM ((ReturnStatement (Just e)): xs) = eval e >>= (\e -> throwError (ReturnException e))

interpretM ((FunStatement name args body) : xs) = do
    (Environment closure _ _) <- get
    let e = Func name (Function args body) closure
    modifyEnvironment (insertEnvironment name e)
    interpretM xs
