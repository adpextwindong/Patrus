{-# LANGUAGE LambdaCase #-}
module Patrus.EvalK where

import Debug.Trace ( trace )
import Data.Time.Clock (diffTimeToPicoseconds, getCurrentTime, UTCTime(utctDayTime))
import qualified Data.Map as M

import Patrus.Env
import Patrus.Types as P
import Patrus.Eval.Pure
import Patrus.Parser

uopTyMismatch = "Operand must be a number."
bopTyMismatch = "Operands must be numbers."
plusTyMismatch = "Operands must be two numbers or two strings."
noCallerContFail = fail "ERROR: Return no caller continuation"

errVarError identifier = error $ "Undefined variable '" <> identifier <> "'."

type Store = Environment --Unify store/env??
type Cont = Store -> Store

--Used for debugging evalK directly
kTraceM :: KM IO
kTraceM e = trace (show e) return

--Monadic Expression Continuation (for printing/etc...)
type KM m = Expr -> Store -> m Store

evalK :: Expr -> KM IO -> (Store -> IO Store)
evalK e@(Lit _) k env = k e env

evalK (Var i) k environ@(Environment env global rk) =
    case lookupEnv i env of
        Nothing -> case lookupEnv i global of
                     Nothing -> errVarError i
                     Just v -> k v environ
        Just v -> k v environ

evalK (Assignment i e) k  env = evalK e (\e' env' -> adjustEnvironmentFM i e' env' >>= k e') env

evalK (NativeFunc Clock []) k env = do
  picoseconds <- fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
  let seconds = Lit $ NumberLit $ picoseconds / 10^^12
  k seconds env

evalK (NativeFunc Clock _) _ _ = fail "Arity Error: Clock NativeFunc expects no args"

evalK (Group e) k env = evalK e k env

--TODO write test of number to bool negation conversion
evalK (UOp Negate e) k env = evalK e (\e' env' -> case e' of
                                            (Lit (NumberLit x)) -> k (Lit (NumberLit (-x))) env'
                                            other -> k (Lit (BoolLit (not . literalTruth $ other))) env') env

--Dispatch to truthy evals
evalK (UOp Not e) k env = evalK e (k . notTruthy) env

evalK (BOp (Cmp P.EQ) e1 e2) k env = evalK e1
  (\e1' env' -> evalK e2 (k . equality P.EQ e1') env') env

evalK (BOp (Cmp P.NEQ) e1 e2) k env = evalK e1
  (\e1' env' -> evalK e2 (k . equality P.NEQ e1') env') env

evalK (BOp And e1 e2) k env = evalK e1
  (\e1' env' -> if not . literalTruth $ e1'
                then k e1' env'
                else evalK e2 k env') env

evalK (BOp Or e1 e2) k env = evalK e1
  (\e1' env' -> if literalTruth e1'
                then k e1' env'
                else evalK e2 k env') env


evalK (BOp operator e1 e2) k env = evalK e1 (\e1' env' -> evalK e2 (tyMatchCont e1') env') env
  where
    tyMatchCont = \e1' e2' env'' -> if not $ sameLitType e1' e2'
                                    then case operator of
                                        Plus -> do
                                          print $ "ATTEMPTED: " <> show operator <> " " <> show e1' <> show e2'
                                          fail plusTyMismatch
                                        _ -> fail bopTyMismatch
                                     else
                                        k (evalBop operator e1' e2') env''

evalK (Call callee args) k environment = evalK callee (\callee' env' ->
  mapEvalK args (\args' env'' ->
  case callee' of
    e@(NativeFunc _ _) -> evalK e k env''
    (Func (Function params body) closure) -> do
                                                callTyCheck callee'
                                                arityCheck params args'
                                                let bindings = zip params args'
                                                --TODO add function name to Func so it can recurse on itself instead of globals
                                                let funcEnv = pushFuncEnvironment bindings (fnRetRestoreEnv env'' k closure)
                                                interpretK [body] return funcEnv --Return isn't used
    _ -> fail "Type Error: can't call a non function"
  )env' ) environment

evalK (Func _ _) _ _ = undefined --TODO
evalK Class _ _ = undefined --TODO

--evalK _ _ _ = undefined

mapEvalK :: [Expr] -> ([Expr] -> Store -> IO Store) -> Store -> IO Store
mapEvalK [] k = k []
mapEvalK (e:es) k = evalK e (\e' -> mapEvalK es (k . (e' :)))

callTyCheck (Func _ _) = return ()
callTyCheck Class = return ()
callTyCheck _ = fail $ "Can only call functions and classes"

arityCheck :: [Identifier] -> [Expr] -> IO ()
arityCheck params args = if length params == length args
                         then return ()
                         else fail $ "Expected " <> show (length params) <> " arguments but got " <> show (length args) <> "."

--Boolean not in the Truthyness of Lox
notTruthy :: Expr -> Expr
notTruthy = \case
  (Lit (BoolLit b)) -> Lit $ BoolLit (not b) --Bools are bools
  (Lit Nil) -> Lit $ BoolLit True            --Nil is falsy
  _ -> Lit $ BoolLit False                   --Everything else is truthy

equality :: ComparrisonOp -> Expr -> Expr -> Expr
equality operator e1 e2 = case operator of
                          P.EQ -> Lit $ BoolLit $ e1 == e2
                          NEQ -> Lit $ BoolLit $ e1 /= e2

interpretK :: Program -> (Store -> IO Store) -> (Store -> IO Store)
--interpretK p k | trace ("DEBUG INTERPRETK " <> show p <> "\n") False = undefined
interpretK [] k = k

interpretK ((PrintStatement e) : xs) k = evalK e (\e' env' -> do
  print ("PRINT: " <> show e')
  interpretK xs k env')

interpretK (DumpStatement : xs) k = \env -> putStr ("DUMP: " <> show env <> "\n") >> interpretK xs k env

interpretK ((ExprStatement e) : xs) k = evalK e (\_ -> interpretK xs k)

interpretK ((VarDeclaration i Nothing) : xs) k = \env -> interpretK xs k (insertEnvironment i (Lit Nil) env)

interpretK (VarDeclaration i (Just e) : xs) k = evalK e (\e' env' -> interpretK xs k (insertEnvironment i e' env'))

interpretK ((FunStatement name params body) : xs) k = \environ ->
  let fn = Func (Function params body) (env environ) in
    interpretK xs k (insertEnvironment name fn environ)

interpretK ((BlockStatement bs) : xs) k = \env -> interpretK bs (\env -> interpretK xs k (popFuncEnvironment env)) (pushFuncEnvironment [] env)

--TODO test IFE xs interpretting
interpretK ((IfStatement conde trueBranch falseBranch) : xs) k = evalK conde (\conde' ->
  if literalTruth conde'
  then interpretK [trueBranch] (interpretK xs k)
  else case falseBranch of
    Just fb -> interpretK [fb] (interpretK xs k)
    Nothing -> interpretK xs k)

--9.3 Challenge TODO break statement continuation stashing in env
interpretK w@((WhileStatement conde body): xs) k = evalK conde (\conde' ->
  if literalTruth conde'
  then interpretK [body] (interpretK w k)
  else interpretK xs k)

--TODO test funcalls and nested funcalls
interpretK (ReturnStatement Nothing: _) _ = \env ->
  case fnReturnK env of
    Nothing -> noCallerContFail
    (Just (fk, callerEnv)) -> fk (Lit Nil) callerEnv

interpretK ((ReturnStatement (Just e)): _) _ = evalK e (\e' env' ->
  case fnReturnK env' of
    Nothing -> noCallerContFail
    (Just (fk, callerEnv)) -> fk e' callerEnv)


baseGlobalEnv :: Env
baseGlobalEnv = Env baseScope EmptyEnv
  where baseScope = M.fromList [clock]
        clock = ("clock", NativeFunc Clock [])


emptyEnvironment = Environment EmptyEnv baseGlobalEnv Nothing

--TODO fix this to include globals
fnRetRestoreEnv :: Environment -> KM IO -> Env -> Environment
fnRetRestoreEnv callerEnv@(Environment _ global _) k closure = Environment closure global (Just (k, callerEnv))

replaceRetGlobal :: Environment -> Environment -> Environment
replaceRetGlobal = undefined

kTraceList :: [Expr] -> Store -> IO Store
kTraceList xs = trace (show xs) return

{- TESTS -}
tprintAdd = interpretK [PrintStatement (termplus 1 2)] return emptyEnvironment
tptrint = interpretK [PrintStatement (Lit (NumberLit 42.0))] return emptyEnvironment

termplus x y = BOp Plus (Lit (NumberLit x)) (Lit (NumberLit y))
testmapevaldk = mapEvalK [termplus 1 2, termplus 3 4, termplus 5 6] kTraceList emptyEnvironment

blockPushPopTest = interpretK prog return emptyEnvironment
  where
    prog = [VarDeclaration "x" Nothing
           ,DumpStatement
           ,BlockStatement [
             VarDeclaration "y" Nothing
             ,DumpStatement
           ]
           ,DumpStatement]

ifetest = interpretK ifeProg return emptyEnvironment
  where ifeProg = parseProgram "var n = 2; if (n<=2) print n; else print 666; var y = 10;"

fibtest = interpretK fib return emptyEnvironment
fib = parseProgram "fun fib(n) { if (n <= 1) { return n; } else { return fib(n-2) + fib(n-1); } } print fib(20);"
