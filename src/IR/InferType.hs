-- Static inference of the type of an closure
module InferType (runInfer, infer, InferState) where

import Syntax(Exp(..), PrimeOp(..))
import Type(Type(..), intersect)

import Control.Monad.Except(ExceptT, throwError, runExceptT)
import Control.Monad.State.Lazy(State, get, gets, modify, evalState)

import Data.Map.Lazy(Map, (!?), insert, delete)

-- An environment for caching known type of a symbol
type TypeMap = Map String Type
-- For saving the expression bound to a symbol
type ExpMap = Map String Exp

-- When a symbol's type is unknown yet, infer the type of the
-- expression bound to it and save the inferred type to avoid later
-- redundant inference. Thus the inference state requires both a type
-- map and an expression map of symbols
type InferState = (TypeMap, ExpMap)

saveType :: String -> Type -> InferState -> InferState
saveType sym t s = (insert sym t $ fst s, snd s)

deleteType :: String -> InferState -> InferState
deleteType sym s = (delete sym $ fst s, snd s)

-- An inference will look up and update a symbol's type, look up a
-- symbol's expression and may fail at type conflict
type Infer = ExceptT String (State InferState)

runInfer :: InferState -> Infer a -> Either String a
runInfer s inf = (evalState . runExceptT) inf s

getTypeMap :: Infer TypeMap
getTypeMap = gets fst


getExpMap :: Infer ExpMap
getExpMap = gets snd
                            

mustIntersect :: Type -> Type -> Infer Type
mustIntersect t0 t1 =
  case t0 `intersect` t1 of
    Nothing -> throwError ("Unmatched type " ++ show t0 ++ " and " ++ show t1)
    Just t -> return t

symbolType :: String -> Infer (Maybe Type)
symbolType sym = getTypeMap >>= (return . (!? sym))

-- Throw error if sym type is unknown
knownSymbolType :: String -> Infer Type
knownSymbolType sym = do tm <- symbolType sym
                         case tm of
                           Just t -> return t
                           Nothing -> throwError ("Symbol " ++ sym ++ "'s type is unknown")


symbolExp :: String -> Infer Exp
symbolExp sym = do em <- getExpMap
                   case em !? sym of
                     Nothing -> throwError ("Unknown symbol " ++ sym)
                     Just e -> return e


-- Main entry of type inference
infer :: Type -> Exp -> Infer Type
infer t (IntLitExp _) = mustIntersect t TyInt
infer t (BoolLitExp _) = mustIntersect t TyBool
infer t (PrimeExp op e0 e1) =
  case op of
    Plus -> inferBinExp TyInt t e0 e1
    Minus -> inferBinExp TyInt t e0 e1
    Times -> inferBinExp TyInt t e0 e1
    And -> inferBinExp TyBool t e0 e1
    Or -> inferBinExp TyBool t e0 e1
    LessThan -> inferRelationExp t e0 e1
    Equal -> inferRelationExp t e0 e1
infer t (NegateExp exp) =
  do infer TyBool exp
     mustIntersect t TyBool
infer t (SymbolExp sym) =
  do stm <- symbolType sym
     st' <- case stm of
              Just st ->
                -- Already known.
                mustIntersect t st
              Nothing -> -- Symbol not inferred yet
                do modify $ saveType sym TyAny
                   -- Assuming sym type is any, so that recursive call
                   -- on sym will stop
                   symbolExp sym >>= infer t
     modify $ saveType sym st'
     return st'
infer t (IfExp cond e0 e1) =
  do infer TyBool cond -- cond must be bool
     t0 <- infer t e0  -- e0 must agree with t0
     t1 <- infer t e1  -- e1 must agree with t1
     t' <- mustIntersect t0 t1
     -- Necessary to infer e0 and e1 again with t' as an assumption,
     -- in order to update any symbol's type with t'
     if t' /= t0
       then infer t' e0
       else return t'
     if t' /= t1
       then infer t' e1
       else return t'
infer t (LambdaExp sym e) =
  -- The type of x in λx.e is undetermined, thus TyAny. The type of λx.e
  -- must agree with TyAny->b, where b is the inferred type of e with x
  -- being any.
  do symtm <- symbolType sym
     -- First, save any existing inference of type if any
     modify $ saveType sym TyAny
     -- Then, overwrite sym's type as undetermined, aka TyAny
     et <- infer TyAny e
     -- Infer e's type under the assumption sym type is TyAny
     symt' <- knownSymbolType sym -- sym's type updated during inference
     modify (case symtm of
               Nothing ->
                 -- There was no sym before, forget sym
                 deleteType sym
               Just symt ->
                 -- Restore sym's original type
                 saveType sym symt)
     mustIntersect t (TyArrow symt' et)
infer t (AppExp a b) =
  do ta <- infer (TyArrow TyAny TyAny) a
     case ta of
       TyArrow tl tr ->
         do infer tl b -- Check b's type
            mustIntersect t tr
       t ->
         throwError ("Expecting an arrow type. But found " ++ show t)
  
--inferBinExp gt t e0 e1 enforces result type t must intersects
--with golden type gt and the two operand expressions must be interred
--to gt
inferBinExp :: Type -> Type -> Exp -> Exp -> Infer Type
inferBinExp gt t e0 e1 =
  do infer gt e0 -- Both e0 and e1 must be inferred
     infer gt e1 -- as golden type.
     mustIntersect gt t -- And t must intersects with int type

inferRelationExp :: Type -> Exp -> Exp -> Infer Type
inferRelationExp t e0 e1 =
  do infer TyInt e0  -- Both e0 and e1 must be inferred
     infer TyInt e1  -- as int type.
     mustIntersect t TyBool -- And t must intersects with bool type
     
