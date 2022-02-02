{-# LANGUAGE LambdaCase #-}
module Patrus.EvalK where

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace ( trace )
import Data.Time.Clock (diffTimeToPicoseconds, getCurrentTime, UTCTime(utctDayTime))
import Control.Monad

import Patrus.Env
import Patrus.Types as P
import Patrus.Eval.Pure

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

evalK (Var i) k (Environment env rk) =
    case lookupEnv i env of
        Nothing -> errVarError i
        Just v -> k v (Environment env rk)

evalK (Assignment i e) k  env = evalK e (\e' env' -> adjustEnvironmentFM i e' env' >>= k e') env

evalK (NativeFunc Clock []) k env = do
  picoseconds <- fromIntegral . diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
  let seconds = Lit $ NumberLit $ picoseconds / 10^^12
  k seconds env

evalK (Group e) k env = evalK e k env

evalK (UOp Negate e) k env = evalK e (\e' env' -> case e' of
                                            (Lit (NumberLit x)) -> k (Lit (NumberLit (-x))) env'
                                            _ -> fail uopTyMismatch) env

--Dispatch to truthy evals
evalK (UOp Not e) k env = evalK e (k . notTruthy) env

evalK (BOp (Cmp P.EQ) e1 e2) k env = evalK e1
  (\e1' env' -> evalK e2 (k . truthy P.EQ e1') env') env

evalK (BOp (Cmp P.NEQ) e1 e2) k env = evalK e1
  (\e1' env' -> evalK e2 (k . truthy P.NEQ e1') env') env

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

evalK (Call callee args) k environ@(Environment env _) = evalK callee (\callee' env' ->
  mapEvalK args (\args' env'' ->
  case callee' of
    e@(NativeFunc _ _) -> evalK e k env''
    e@(Func (Function params body) closure) -> do
                                                callTyCheck callee'
                                                arityCheck params args'
                                                interpretK [body] return (injectFnRet env'' k) --Return isn't used
  )env' ) environ

evalK _ _ _ = undefined

mapEvalK [] k = k []
mapEvalk xs k = mapEvalK' xs [] k

mapEvalK' [] evalds k = k evalds
mapEvalK' (e:es) evalds k = \env -> evalK e (\e' -> mapEvalK' es (e' : evalds) k) env

callTyCheck e@(Func _ _) = return ()
callTyCheck e@(Class) = return ()
callTyCheck e = fail $ "Can only call functions and classes"

arityCheck params args = undefined

--Boolean not in the Truthyness of Lox
notTruthy :: Expr -> Expr
notTruthy = \case
  (Lit (BoolLit b)) -> Lit $ BoolLit (not b) --Bools are bools
  (Lit Nil) -> Lit $ BoolLit True            --Nil is falsy
  e -> Lit $ BoolLit False                   --Everything else is truthy

truthy :: ComparrisonOp -> Expr -> Expr -> Expr
truthy operator e1 e2 = case operator of
                          P.EQ -> Lit $ BoolLit $ literalTruth e1 == literalTruth e2
                          NEQ -> Lit $ BoolLit $ literalTruth e1 /= literalTruth e2

interpretK :: Program -> (Store -> IO Store) -> (Store -> IO Store)
interpretK [] k = k

interpretK ((PrintStatement e) : xs) k = evalK e (\e' env' -> do
  print ("PRINT: " <> show e')
  interpretK xs k env')

interpretK (DumpStatement : xs) k = \env -> print ("DUMP: " <> show env) >> interpretK xs k env

interpretK ((ExprStatement e) : xs) k = evalK e (\_ -> interpretK xs k)

interpretK ((VarDeclaration i Nothing) : xs) k = \env -> interpretK xs k (insertEnvironment i (Lit Nil) env)

interpretK (VarDeclaration i (Just e) : xs) k = evalK e (\e' env' -> interpretK xs k (insertEnvironment i e' env'))

interpretK ((BlockStatement bs) : xs) k = \env -> interpretK bs (interpretK xs k) (pushFuncEnvironment [] env)

interpretK ((IfStatement conde trueBranch falseBranch) : xs) k = evalK conde (\conde' ->
  if literalTruth conde'
  then interpretK [trueBranch] k
  else case falseBranch of
    Just fb -> interpretK [fb] k
    Nothing -> k)

--9.3 Challenge TODO break statement continuation stashing in env
interpretK w@((WhileStatement conde body): xs) k = evalK conde (\conde' ->
  if literalTruth conde'
  then interpretK [body] (interpretK w k)
  else interpretK xs k)

--TODO test funcalls and nested funcalls
interpretK (ReturnStatement Nothing: _) _ = \env ->
  case fnReturnK env of
    Nothing -> noCallerContFail
    Just fnk -> fnk (Lit Nil) (popFnRet env)

interpretK ((ReturnStatement (Just e)): _) _ = evalK e (\e' env' ->
  case fnReturnK env' of
    Nothing -> noCallerContFail
    Just fnk -> fnk e' (popFnRet env'))

tprintAdd = interpretK [PrintStatement oneplustwo] return emptyEnvironment
tptrint = interpretK [PrintStatement (Lit (NumberLit 42.0))] return emptyEnvironment

emptyEnvironment = Environment EmptyEnv Nothing

injectFnRet (Environment env _) k = Environment env (Just k)

popFnRet :: Environment -> Environment
popFnRet (Environment env _) = Environment env Nothing

oneplustwo = (BOp Plus (Lit (NumberLit 1.0)) (Lit (NumberLit 2.0)))
testmapevaldk = mapEvalk (take 5 (repeat oneplustwo)) kTraceList emptyEnvironment

kTraceList :: [Expr] -> Store -> IO Store
kTraceList xs = trace (show xs) return
