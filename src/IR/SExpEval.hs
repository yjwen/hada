module SExpEval (runProg) where
import SExpSyntax (Prog(..), DefineStat(..), Exp(..))

import Data.Map.Strict(Map, (!?), insert)
import Control.Monad.State.Lazy(State, evalState, lift, get, put)
import Control.Monad.Trans.Except(ExceptT, runExceptT, throwE)

type Value = Either Exp Int
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

eval :: Exp -> Eval Int
eval (ValueExp i) = success i
eval (SymbolExp sym) = do
  v <- extract sym
  case v of
    Right i -> success i
    Left exp -> eval exp

eval (PrimeExp op exp0 exp1) = do
  v0 <- eval exp0
  v1 <- eval exp1
  let f = case op of
        '+' -> (+)
        '-' -> (-)
        otherwise -> (*)
  success (f v0 v1)

def :: DefineStat -> Eval Value
def (DefineStat sym exp) = update sym (Left exp)

runProg :: Env -> Prog -> Either String Int
runProg env (Prog defs exp) = (evalState . runExceptT) act env
  where act = mapM def defs >> eval exp
