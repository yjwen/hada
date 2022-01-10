module Eval (runProg) where

import Syntax (PrimeOp(..), Prog(..), DefineStat(..), Exp(..))

import Data.Map.Strict(Map, (!?), insert)
import Control.Monad.State.Lazy(State, evalState, lift, get, put)
import Control.Monad.Trans.Except(ExceptT, runExceptT, throwE)

type Env = Map String Exp
-- Mapping symbol name to its value, which is either an expression or its
-- evaluation result
type Eval = ExceptT String (State Env)
-- An evaluation updates symbol value and may fail with an error message

-- Extract a symbol's value. If no such symbol is found, error with an
-- message
extract :: String -> Eval Exp
extract sym = do
  env <- lift get
  case env !? sym of
    Nothing -> throwE ("Symbol " ++ sym ++ " is not defined")
    Just rhs -> return rhs

getEnv :: Eval Env
getEnv = lift get

-- Update symbol's value to the evaluation result
update :: String -> Exp -> Eval Exp
update sym value = do
  env <- getEnv
  put (insert sym value env)
  return value

doEval :: Eval Exp -> Env -> Either String Exp
doEval = evalState . runExceptT

eval :: Exp -> Eval Exp
eval v@(IntLitExp i) = return v
eval v@(BoolLitExp b) = return v
eval (SymbolExp sym) = do
  v <- extract sym
  case v of
    IntLitExp _ -> return v
    BoolLitExp _ -> return v
    LambdaExp _ _ -> return v
    otherwise -> eval v >>= update sym

eval (PrimeExp op exp0 exp1) = do
  v0 <- eval exp0
  v1 <- eval exp1
  case op of
    Plus -> evalInt (+) v0 v1 IntLitExp
    Minus -> evalInt (-) v0 v1 IntLitExp
    Times -> evalInt (*) v0 v1 IntLitExp
    LessThan -> evalInt (<) v0 v1 BoolLitExp
    Equal -> evalInt (<) v0 v1 BoolLitExp
    And -> evalBool (&&) v0 v1
    Or -> evalBool (||) v0 v1

eval (IfExp pred expT expF) = do
  b <- eval pred >>= extractBool
  if b
    then eval expT
    else eval expF
  
eval (NegateExp e) = eval e >>= extractBool >>= (\b -> return $ BoolLitExp (not b))

eval e@(LambdaExp _ _) = return e -- Do not evaluate lambda expression
                                  -- until it is applied

eval (AppExp e0 e1) = do
  f <- eval e0
  (sym, exp) <- extractLambda f
  env <- getEnv
  case doEval (eval exp) (insert sym e1 env) of
    Left err -> throwE err
    Right exp -> return exp
  

extractInt :: Exp -> Eval Int
extractInt (IntLitExp i) = return i
extractInt v = throwE ("Expecting an int value, but found" ++ show v)

extractBool :: Exp -> Eval Bool
extractBool (BoolLitExp b) = return b
extractBool v = throwE ("Expecting a bool value, but found" ++ show v)

extractLambda :: Exp -> Eval (String, Exp)
extractLambda (LambdaExp sym exp) = return (sym, exp)
extractLambda v = throwE ("Expecting a lambda value, but found" ++ show v)


evalInt :: (Int -> Int -> a) -> Exp -> Exp -> (a -> Exp) -> Eval Exp
evalInt f v0 v1 c =
  do i0 <- extractInt v0
     i1 <- extractInt v1
     return $ c (f i0 i1)

evalBool :: (Bool -> Bool -> Bool) -> Exp -> Exp -> Eval Exp
evalBool f v0 v1 =
  do b0 <- extractBool v0
     b1 <- extractBool v1
     return $ BoolLitExp (f b0 b1)

def :: DefineStat -> Eval Exp
def (DefineStat sym exp) = update sym exp

runProg :: Env -> Prog -> Either String Exp
runProg env (Prog defs exp) = doEval act env
  where act = mapM def defs >> eval exp
