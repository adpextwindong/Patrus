{-# LANGUAGE DoAndIfThenElse #-}
module Patrus.Eval where

import Debug.Trace
import GHC.Float
import Prelude hiding (EQ,LT,GT)
import Data.Time.Clock

import Control.Monad.IO.Class
import Control.Monad.State.Class (MonadState (..))

import Patrus.Environment
import Patrus.Types

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
    env <- get
    case lookupEnv i env of
        Nothing -> fail $ runtimeVarError i
        Just v -> return v

eval (Assignment i e) = do
   e' <- eval e
   put =<< adjustEnvFM i e' =<< get
   return e'

eval (NativeFunc Clock []) = do
    picoseconds <- liftIO $ fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
    let seconds = picoseconds / fromIntegral 10^^12

    return $ Lit $ NumberLit $ seconds


eval (Call callee args) = do
    callee' <- eval callee
    args' <- mapM eval args
    --TODO typecheck callee
    case callee' of
        e@(NativeFunc _ _ ) -> eval e
        e@(Func (Function params body)) -> do
            callTyCheck callee' --Maybe hoist this to case so we can split callee into pieces
            arityCheck params args'
            let bindings = undefined --TODO zip params and args section 10.4
            (retVal, _) <- withFuncEnv bindings $ interpretM [body]
            return retVal

eval (Group e) = eval e
eval (UOp Negate e) = do
    e' <- eval e
    case e' of
        (Lit (NumberLit x)) -> pure $ Lit $ NumberLit (-x)
        _ -> fail uopTyMismatch

--Dispatch to truthy evals
eval (UOp Not e) = evalNotTruthy e
eval (BOp (Cmp EQ) e1 e2) = evalTruthy EQ e1 e2
eval (BOp (Cmp NEQ) e1 e2) = evalTruthy NEQ e1 e2
eval (BOp And e1 e2) = evalTruthyShortCircuit And e1 e2
eval (BOp Or e1 e2) = evalTruthyShortCircuit Or e1 e2

--Strict Type Matching operators
-- Sub expressions need to be evaluated and type mismatches should be handled.
eval (BOp operator e1 e2) = do
    (e1,e2) <- literalBopTyMatch operator e1 e2
    evalBop operator e1 e2

-- Handles all nonTruthy operators
evalBop :: BinOp -> Expr -> Expr -> EvalM Expr
evalBop (Cmp LT)  (Lit (NumberLit a)) (Lit (NumberLit b)) = pure $ Lit $ BoolLit $ a < b
evalBop (Cmp LTE) (Lit (NumberLit a)) (Lit (NumberLit b)) = pure $ Lit $ BoolLit $ a <= b
evalBop (Cmp GT)  (Lit (NumberLit a)) (Lit (NumberLit b)) = pure $ Lit $ BoolLit $ a > b
evalBop (Cmp GTE) (Lit (NumberLit a)) (Lit (NumberLit b)) = pure $ Lit $ BoolLit $ a >= b

evalBop Plus (Lit (StringLit s1)) (Lit (StringLit s2)) = pure $ Lit $ StringLit (s1 <> s2)
evalBop Plus (Lit (NumberLit x)) (Lit (NumberLit y))   = pure $ Lit $ NumberLit (x + y)
evalBop Minus (Lit (NumberLit x)) (Lit (NumberLit y))  = pure $ Lit $ NumberLit (x - y)
evalBop Mul (Lit (NumberLit x)) (Lit (NumberLit y))    = pure $ Lit $ NumberLit (x * y)
evalBop Div (Lit (NumberLit x)) (Lit (NumberLit y))    = pure $ Lit $ NumberLit (x / y)

evalBop _ _ _ = fail $ bopTyMismatch
--evalBop op e1 e2 = trace ("EXHAUST: "<> show op <> " " <> show e1 <> " " <> show e2) $ undefined

-- | Performs strict type matching for PLUS/MINUS/DIV/MUL/LT/LTE/GT/GTE
-- Nil in any operand causes an error, EQ and NEQ should be handled earlier.
literalBopTyMatch :: BinOp -> Expr -> Expr -> EvalM (Expr,Expr)
literalBopTyMatch operator e1 e2 = do
    e1 <- eval e1
    e2 <- eval e2
    if not $ sameLitType e1 e2
    then case operator of
            Plus -> fail plusTyMismatch
            _    -> fail bopTyMismatch
    else
        pure (e1,e2)

sameLitType :: Expr -> Expr -> Bool
sameLitType (Lit (NumberLit _)) (Lit (NumberLit _)) = True
sameLitType (Lit (StringLit _)) (Lit (StringLit _)) = True
sameLitType (Lit (BoolLit _)) (Lit (BoolLit _)) = True
sameLitType _ _ = False

callTyCheck :: Expr -> EvalM Expr
callTyCheck e@(Func _) = return e
callTyCheck e@(Class) = return e
callTyCheck e = fail $ "Can only call functions and classes."

arityCheck :: [Identifier] -> [Expr] -> EvalM ()
arityCheck params args = if length params == length args
                         then return ()
                         else fail $ "Expected " <> show (length params) <> " arguments but got " <> show (length args) <> "."

evalTruthy :: ComparrisonOp -> Expr -> Expr -> EvalM Expr
evalTruthy operator e1 e2 = do
   e1 <- eval e1
   e2 <- eval e2
   return $ case operator of
        EQ -> Lit $ BoolLit $ literalTruth e1 == literalTruth e2
        NEQ -> Lit $ BoolLit $ literalTruth e1 /= literalTruth e2

evalNotTruthy :: Expr -> EvalM Expr
evalNotTruthy e = do
    e' <- eval e
    return $ case e' of
        (Lit (BoolLit b)) -> Lit $ BoolLit (not b)    --Bools are bools
        (Lit Nil) -> Lit $ BoolLit True               --Nil is falsy
        e -> Lit $ BoolLit False                      --Every else is truthy

evalTruthyShortCircuit And e1 e2 = do
    e1' <- eval e1
    if not . literalTruth $ e1'
    then return e1'
    else do
        e2' <- eval e2
        return e2'

evalTruthyShortCircuit Or e1 e2 = do
    e1' <- eval e1
    if literalTruth e1'
    then return e1'
    else do
        e2' <- eval e2
        return e2'

literalTruth :: Expr -> Bool
literalTruth (Lit Nil) = False
literalTruth (Lit (BoolLit False)) = False
literalTruth _ = True

-- interpretFunctionM :: Function -> EvalM Expr
-- interpretFunctionM

interpretM :: Program -> EvalM (Expr, Program)
interpretM [] = return (Unit,[])
interpretM ((PrintStatement e): xs) = do
    e' <- eval e
    liftIO $ print $ "PRINT: " <> show e'
    interpretM xs

interpretM ((DumpStatement) : xs) = do
    env <- get
    liftIO $ print $ "DUMP: " <> show env
    interpretM xs

interpretM ((ExprStatement e) : xs) = do
    _ <- eval e
    interpretM xs

interpretM ((VarDeclaration i Nothing) : xs) = do
    modifyEnv $ insertEnv i (Lit Nil)
    interpretM xs

interpretM (VarDeclaration i (Just e) : xs) = do
    e' <- eval e
    modifyEnv (insertEnv i e')
    interpretM xs

interpretM ((BlockStatement bs):xs) = do
    withFreshEnv (interpretM bs)
    interpretM xs

interpretM ((IfStatement conde trueBranch falseBranch): xs) = do
    e <- eval conde

    if literalTruth e
    then interpretM [trueBranch]
    else case falseBranch of
        Just fb -> interpretM [fb]
        Nothing -> pure (Unit,[])

    interpretM xs

interpretM w@((WhileStatement conde body):xs) = do
    e <- eval conde

    if literalTruth e
    then interpretM [body] >> interpretM w
    else interpretM xs

interpretM ((ReturnStatement Nothing): xs) = return (Lit Nil, xs)
interpretM ((ReturnStatement (Just e)): xs) = do
    e' <- eval e
    return (e', xs)

interpretM ((FunStatement name args body) : xs) = trace ("TODO function declaration 10.4 10.4.1 ") undefined
