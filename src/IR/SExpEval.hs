module SExpEval (runProg) where

import SExpTokens (PrimeOp(..))
import SExpSyntax (Prog(..), DefineStat(..), Exp(..))

import Data.Map.Strict(Map, (!?), insert)
import Control.Monad.State.Lazy(State, evalState, lift, get, put)
import Control.Monad.Trans.Except(ExceptT, runExceptT, throwE)

data Value = ExpV Exp
           | IntV Int
           | BoolV Bool
           deriving Show
type Env = Map String Value
-- Mapping symbol name to its value, which is either an expression or its
-- evaluation result
type Eval = ExceptT String (State Env)
-- An evaluation updates symbol value and may fail with an error message

-- Evaluation done without error and with a result
success :: a -> Eval a
success = lift . return

-- Extract a symbol's value. If no such symbol is found, error with an
-- message
extract :: String -> Eval Value
extract sym = do
  env <- lift get
  case env !? sym of
    Nothing -> throwE ("Symbol " ++ sym ++ " is not defined")
    Just rhs -> success rhs

-- Update symbol's value to the evaluation result
update :: String -> Value -> Eval Value
update sym value = do
  env <- lift get
  put (insert sym value env)
  success value

eval :: Exp -> Eval Value
eval (IntLitExp i) = success (IntV i)
eval (BoolLitExp b) = success (BoolV b)
eval (SymbolExp sym) = do
  v <- extract sym
  case v of
    IntV _ -> success v
    BoolV _ -> success v
    ExpV exp -> eval exp

eval (PrimeExp op exp0 exp1) = do
  v0 <- eval exp0
  v1 <- eval exp1
  case op of
    Plus -> evalInt (+) v0 v1 IntV
    Minus -> evalInt (-) v0 v1 IntV
    Times -> evalInt (*) v0 v1 IntV
    LessThan -> evalInt (<) v0 v1 BoolV
    Equal -> evalInt (<) v0 v1 BoolV
    And -> evalBool (&&) v0 v1
    Or -> evalBool (||) v0 v1
  
eval (NegateExp e) = eval e >>= extractBool >>= (\b -> success $ BoolV (not b))

extractInt :: Value -> Eval Int
extractInt (IntV i) = success i
extractInt v = throwE ("Expecting an int value, but found" ++ show v)

evalInt :: (Int -> Int -> a) -> Value -> Value -> (a -> Value) -> Eval Value
evalInt f v0 v1 c =
  do i0 <- extractInt v0
     i1 <- extractInt v1
     success $ c (f i0 i1)

extractBool :: Value -> Eval Bool
extractBool (BoolV b) = success b
extractBool v = throwE ("Expecting an bool value, but found" ++ show v)

evalBool :: (Bool -> Bool -> Bool) -> Value -> Value -> Eval Value
evalBool f v0 v1 =
  do b0 <- extractBool v0
     b1 <- extractBool v1
     success $ BoolV (f b0 b1)

def :: DefineStat -> Eval Value
def (DefineStat sym exp) = update sym (ExpV exp)

runProg :: Env -> Prog -> Either String Value
runProg env (Prog defs exp) = (evalState . runExceptT) act env
  where act = mapM def defs >> eval exp
