module DFGSyn where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified CoreSyn as C
import Outputable
import PprCore
import Data.List (intercalate, find)
import Data.Bits (shift, finiteBitSize)
import qualified Data.Map.Lazy as LM
import Literal
import Type
import TyCon
import Var
import Name (getOccString, getName, getOccName)
import DataCon (DataCon, dataConTag, dataConOrigArgTys, dataConEqSpec, dataConTyCon)
import Data.Functor.Identity
  
import DFG

showppr :: Outputable a => a -> String
showppr = showSDocUnsafe . ppr

type LocalGraphs = LM.Map String Graph

type GraphSE = ExceptT String GraphS -- State monad of Graph with exceptions

type StringE = Except String -- Exceptional values that don't involve Graph

mapE :: StringE a -> GraphSE a -- Lift the exceptional value to GraphSE
mapE = mapExceptT (return . runIdentity)

withE ::  Maybe a -> String -> StringE a
withE Nothing e = throwE e
withE (Just a) _ = return a

expr :: Maybe Signal -> LocalGraphs -> C.CoreExpr -> GraphSE Signal
expr s l (C.Case e v t as) =
    do cond <- expr Nothing l e
       stype <- mapE $ getType t
       o <- lift $ autoSignal s stype
       (dflt, branches) <- alts l as
       case dflt of
         Just dflt' -> lift $ insertNode (CaseNode o cond dflt' branches)
         Nothing -> if length branches == (shift 1 $ typeWidth $ signalType cond)
                    then lift $ insertNode $ CaseNode o cond (snd $ head branches) (tail branches)
                    else error "Incomplete branches"
       return o

expr s l app@(C.App _ arg) =
    case C.collectArgs app of
      (C.Var v, args) -> varApp s l v args
      (C.Lam lv lexp, [C.Var larg]) ->
          if isVoidTy $ varType larg
          -- larg's type is void, the lambda
          -- is merely a wrapper of lexp,
          -- continue to synthesize lexp.
          then expr s l lexp
          else error $ "Unsupported lambda application. " ++ (showppr app)
      _ -> error $ "Unsupported application. " ++ (showppr app)

expr _ _ (C.Var v) =
  do s <- mapE $ mkVarSignal v
     lift $ insertSignal s


expr s l (C.Lam v e) =
    if isVoidTy $ varType v
    -- v's type is void, the lambda has no argument, continue to
    -- synthesis the lambda's expression.
    then expr s l e
    else error $ "Unsupported lambda expression."

expr s l (C.Let bind exp) =  do b <- mapE $ localBind l bind
                                case b of
                                  Just graph -> expr s (LM.insert (graphName graph) graph l) exp
                                  Nothing -> expr s l exp

expr _ _ e = error $ "Unsupported Node type: " ++ showppr e


splitArgs :: [C.CoreExpr] -> ([Type], [C.CoreExpr])
splitArgs ((C.Type tyarg):args) = let (tyargs, args') = splitArgs args
                                  in (tyarg:tyargs, args')
splitArgs args = ([], args)

specialize :: Type -> [C.CoreExpr] -> (Type, [C.CoreExpr])
specialize typ args
    = let (tyvars, typ') = splitForAllTys typ
          (tyargs, vargs) = splitArgs args
          tvsubs = zipTvSubst tyvars tyargs
      in (substTy tvsubs typ', vargs)

remove_predicates :: Type -> [C.CoreExpr] -> (Type, [C.CoreExpr])
remove_predicates typ (arg:args)
  | Just (t, f) <- Type.splitFunTy_maybe typ
  , Type.isPredTy t
  = remove_predicates f args
remove_predicates typ args = (typ, args)


getDataCon :: Var -> Maybe DataCon
getDataCon v
  | Just (tc, _) <- splitTyConApp_maybe $ snd $ splitForAllTys $ Var.varType v
  , Just dcs <- tyConDataCons_maybe tc
  = find ((getOccString v ==) . getOccString) dcs

getDataCon _ = Nothing

varApp :: Maybe Signal -> LocalGraphs -> Var -> [C.CoreExpr] ->  GraphSE Signal
varApp s l v args
    | Just op <- binOp v
    , (typ, [opl, opr]) <- uncurry remove_predicates $ specialize (Var.varType v) args
    -- The application is a binary expression.
    = do let (argTypes, resultType) = splitFunTys typ
         lsig <- expr Nothing l opl
         rsig <- expr Nothing l opr
         stype <- mapE $ getType resultType
         o <- lift $ autoSignal s stype
         lift $ insertNode $ BinNode o op lsig rsig
         return o
varApp s l v args
  | (t, args') <- specialize (Var.varType v) args
  , Just (l, r) <- Type.splitFunTy_maybe t
  = throwE $ "DEBUG: l=" ++ showppr l ++ ", ispred=" ++ (show $ Type.isPredTy l) ++ ", r=" ++ showppr r ++ ", ispred=" ++ (show $ Type.isPredTy r) ++ (showppr $ remove_predicates t args')

varApp s l v args
  | Just dc <- getDataCon v
  = throwE $ "Unsupported varApp dataCon, dc=" ++ showppr dc ++ ", tycon=" ++ (showppr $ dataConTyCon dc)

varApp _ _ v args = throwE $ "Unsupported varApp, getName v=" ++ (showppr $ getName v) ++ ", args=" ++ showppr args ++ ", t=" ++ showppr t ++ ", tc_m=" ++ showppr tc_m ++ ", dcs_m=" ++ showppr dcs_m ++ ", getDataCon v=" ++ showppr (getDataCon v)
  where t = snd $ splitForAllTys $ Var.varType v
        tc_m = fmap fst $ splitTyConApp_maybe t
        (t', _) = specialize (Var.varType v) args
        dcs_m = fmap (fmap (map getName) . tyConDataCons_maybe) tc_m

binOp :: Var -> Maybe BinOp
binOp var = case getOccString $ Var.varName var of
                "<" -> Just LessThan
                ">" -> Just GreaterThan
                "-" -> Just Minus
                otherwise -> Nothing


alts :: LocalGraphs -> [C.Alt Var] -> GraphSE (Maybe Signal, [(Integer, Signal)])
alts _ [] = do return (Nothing, [])
alts l ((altCon, vars, exp):as) =
    do (dflt, branches) <- alts l as
       sig <- expr Nothing l exp
       case altCon of
         C.LitAlt lit -> case lit of
                           MachInt i -> return (dflt, (i, sig):branches)
                           _ -> error $ "Unsupported lit " ++ showppr lit
         C.DEFAULT -> return (Just sig, branches)
         C.DataAlt dc -> let tag = dataConTag dc
                         in return (dflt, (toInteger $ tag - 1, sig):branches)

bind :: C.CoreBind -> StringE (Maybe Graph)
bind (C.NonRec b e)
  |  "$trModule" <- getOccString b -- The module bind, ignore for now.
  = return Nothing
bind b = localBind LM.empty b

localBind :: LocalGraphs -> C.CoreBind -> StringE (Maybe Graph)
localBind l (C.NonRec b e) =
  let (_, outputType) = splitFunTys $ varType b
      (decurriedExpr, inputVars) = decurry e []
      moduleName = getOccString b
      syn = do out <- mapE $ mkSignal outputType "out"
               lift $ insertOutputSignal out
               expr (Just out) l decurriedExpr
      (ret, g) = runState (runExceptT syn) $ emptyGraph moduleName
  in case ret of
    Right _ -> return (Just g)
    Left err -> throwE err

localBind _ (C.Rec _) = throwE "Cannot translate C.Rec"

decurry :: C.CoreExpr -> [Var] -> (C.CoreExpr, [Var])
decurry (C.Lam v e) as = (dexp, v:vs)
  where (dexp, vs) = decurry e as
decurry e as = (e, as)


mkVarSignal :: Var -> StringE Signal
mkVarSignal v = do let n = Left $ getOccString $ Var.varName v
                   sigType <- getType (Var.varType v)
                   return $ Signal n sigType

mkSignal :: Type -> String -> StringE Signal
mkSignal t n = do sigType <- getType t
                  return $ Signal (Left n) sigType

getType :: Type -> StringE SignalType
getType t | Just (tc, ts) <- splitTyConApp_maybe t
          , isAlgTyCon tc
    = case getOccString $ getName tc of
        "Word" -> return $ SimpleSigType $ finiteBitSize (0::Word)
        "Bool" -> return $ SimpleSigType 1
        "Int" -> return $ SimpleSigType $ finiteBitSize (0::Int)
        otherwise -> getAlgType t

getType t = throwE $ "Unsupported type " ++ showppr t


getAlgType :: Type -> StringE SignalType
getAlgType t | Just (tc, ts) <- splitTyConApp_maybe t
             , isAlgTyCon tc
  = do sigTypes <- mapM getType ts
       let tvs = tyConTyVars tc
       tvSizes <- if length tvs /= length sigTypes
                  then throwE $ "Unmatched type variable and type arguments"
                  else return $ zip tvs $ map typeWidth sigTypes
       dcs <- tyConDataCons_maybe tc `withE` ("No data constructor for type constructor" ++ showppr tc)
       dcSizes <- mapM (getDataConSize tvSizes) dcs
       sigTypeName <- getTypeName t
       case length dcs of
         1 -> return $ SimpleSigType $ maximum dcSizes
         2 -> return $ AlgebraicType sigTypeName 1 $ maximum dcSizes
         otherwise -> return $ AlgebraicType sigTypeName (length dcSizes) $ maximum dcSizes

getDataConSize :: [(TyVar, Int)] -> DataCon -> StringE Int
getDataConSize tvSizes dc =
  do dcVars <- mapM ((`withE` "Type has no type variable") . getTyVar_maybe) $ dataConOrigArgTys dc
     dcSizes <- mapM ((`withE` "Type variable size not found") . (`lookup` tvSizes)) dcVars
     return $ foldl (+) 0 dcSizes

--- isVoidTy _ = False -- Obsolete in GHC 8.2. Finding replacement.

getTypeName :: Type -> StringE [String]
getTypeName t | Just (tc, ts) <- splitTyConApp_maybe t
  = do tsNames <- mapM getTypeName ts
       return ((getOccString $ getName tc) : concat tsNames)
getTypeName t = throwE $ "Don't know how to get name for type" ++ showppr t
