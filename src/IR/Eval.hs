module Eval(runProg) where

import Syntax( PrimeOp(..), Prog(..), DefineStat(..), Exp(..)
             , reducible)
import Heap(Heap, access, insertHeap, newAlloc)
import Env(Env, HeapAddr, symbolAddr, insertEnv)
import Closure(Closure(..), justExp, clExp, clEnv)

import Control.Monad.Except(ExceptT, throwError, runExceptT)
import Control.Monad.State.Lazy(State, modify, get, put, evalState)

type Eval = ExceptT String (State Heap)
-- An evaluation requires an heap and may fail with exception

extractJust :: String -> Maybe a -> Eval a
extractJust err m = case m of
                      Nothing -> throwError err
                      Just x -> return x

accessHeap :: HeapAddr -> Eval Closure
accessHeap addr =
  do heap <- get
     extractJust ("Heap address " ++ show addr ++ " is invalid") (access heap addr) 

evalSym :: Env -> String -> Eval Closure
evalSym env sym =
  do addr <- extractJust ("Symbol " ++ sym ++ " is not allocated") (symbolAddr env sym) 
     symcl <- accessHeap addr
     if reducible (clExp symcl)
        -- Reducible closure, update the heap with reduced one
       then eval (clEnv symcl <> env) (clExp symcl) >>= update addr
       else return symcl

-- Update symbol's value to the evaluation result
update :: HeapAddr -> Closure -> Eval Closure
update addr c = modify (insertHeap addr c) >> return c

-- Assign a new heap address to a closure
alloc :: Closure -> Eval HeapAddr
alloc c = do h <- get
             let (h', addr) = newAlloc c h
             put h'
             return addr

doEval :: Heap -> Eval Closure -> Either String Closure
doEval heap eval = (evalState . runExceptT) eval $ heap

-- Dispatcher for different closure constructs      
eval :: Env -> Exp -> Eval Closure
eval env e = case e of
  -- Integer and boolean literals are not reducible
  IntLitExp _ -> return $ justExp e
  BoolLitExp _ -> return $ justExp e
  SymbolExp sym -> evalSym env sym
  PrimeExp op exp0 exp1 -> evalPrime env op exp0 exp1
  IfExp pred expT expF -> evalIf env pred expT expF
  NegateExp e -> evalNegate env e
  e@(LambdaExp _ _) -> return $ Closure e env
  -- Do not evaluate lambda expression until it is applied, but return
  -- it with the current environment as a closure
  AppExp e0 e1 -> evalApp env e0 e1

evalPrime :: Env -> PrimeOp -> Exp -> Exp -> Eval Closure
evalPrime env op exp0 exp1 =
  do cl0 <- eval env exp0
     cl1 <- eval env exp1
     let v0 = clExp cl0
         v1 = clExp cl1
     -- No more evaluation by cl0 or cl1's environment
     resultExp <- case op of
                    Plus -> evalInt (+) v0 v1 IntLitExp
                    Minus -> evalInt (-) v0 v1 IntLitExp
                    Times -> evalInt (*) v0 v1 IntLitExp
                    LessThan -> evalInt (<) v0 v1 BoolLitExp
                    Equal -> evalInt (==) v0 v1 BoolLitExp
                    And -> evalBool (&&) v0 v1
                    Or -> evalBool (||) v0 v1
     return $ justExp resultExp

evalIf :: Env -> Exp -> Exp -> Exp -> Eval Closure
evalIf env pred expT expF =
  do b <- eval env pred >>= (extractBool . clExp)
     if b
       then eval env expT
       else eval env expF
  
evalNegate :: Env -> Exp -> Eval Closure
evalNegate env e = (eval env e >>=
                    (extractBool . clExp) >>=
                    (return . justExp . BoolLitExp . not))

evalApp :: Env -> Exp -> Exp -> Eval Closure
evalApp env e0 e1 = do
  -- Evaluate e0 in current environment, make sure it reduces to a
  -- closure of lambda
  c0 <- eval env e0
  (sym, exp) <- extractLambda $ clExp c0
  addr <- alloc $ Closure e1 env
  eval (insertEnv sym addr $clEnv c0) exp

extractInt :: Exp -> Eval Int
extractInt (IntLitExp i) = return i
extractInt v = throwError ("Expecting an int value, but found " ++ show v)

extractBool :: Exp -> Eval Bool
extractBool (BoolLitExp b) = return b
extractBool v = throwError ("Expecting a bool value, but found " ++ show v)

extractLambda :: Exp -> Eval (String, Exp)
extractLambda (LambdaExp sym exp) = return (sym, exp)
extractLambda v = throwError ("Expecting a lambda value, but found " ++ show v)


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

-- Reserve an address on heap for the define statement's closure,
-- and map the symbol to that allocation in env
def :: (Env, Heap, [(HeapAddr, Exp)]) -> DefineStat -> (Env, Heap, [(HeapAddr, Exp)])
def (env, heap, maps) (DefineStat sym exp) =
  let (heap', addr) = newAlloc (justExp exp) heap
      env' = insertEnv sym addr env
  in (env', heap', (addr, exp):maps)

-- Allocate heap address for all the top-level definition, update the
-- closure with top-level environment
topDefs :: [DefineStat] -> (Env, Heap) -> (Env, Heap)
topDefs defs (env, heap) =
  let (env', heap', maps) = foldl def (env, heap, []) defs
      -- Now modify the closures at addrs to having env' as their
      -- environtments
      heap'' = foldl (\h (addr, sym) -> insertHeap addr (Closure sym env') h) heap' maps
  in (env', heap'')

runProg :: Env -> Heap -> Prog -> Either String Closure
runProg env heap (Prog defs exp) =
  let (env', heap') = topDefs defs (env, heap)
  in doEval heap' (eval env' exp)
