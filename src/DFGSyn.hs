module DFGSyn where
import Control.Monad.Trans.State
import Control.Monad.Except
import qualified CoreSyn as C
import Outputable
import PprCore
import Data.List (intercalate)
import Data.Bits (shift, finiteBitSize)
import qualified Data.Map.Lazy as LM
import Literal
import Type
import TyCon
import Var
import Name (getOccString, getName)
import DataCon (dataConTag)

import DFG

showppr :: Outputable a => a -> String
showppr = showSDocUnsafe . ppr

type LocalGraphs = LM.Map String Graph

type GraphSE = ExceptT String GraphS -- State monad of Graph with exceptions

expr :: Maybe Signal -> LocalGraphs -> C.CoreExpr -> GraphSE Signal
expr s l (C.Case e v t as) =
    do cond <- expr Nothing l e
       b <- case (getTypeBits t) of
              Right bit -> return bit
              Left err -> throwError err
       o <- lift $ autoSignal s b
       (dflt, branches) <- alts l as
       case dflt of
         Just dflt' -> lift $ insertNode (CaseNode o cond dflt' branches)
         Nothing -> if length branches == (1 `shift` signalWidth cond)
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
  do s <- ExceptT $ return $ mkVarSignal v
     lift $ insertSignal s


expr s l (C.Lam v e) =
    if isVoidTy $ varType v
    -- v's type is void, the lambda has no argument, continue to
    -- synthesis the lambda's expression.
    then expr s l e
    else error $ "Unsupported lambda expression."

expr s l (C.Let bind exp) =
                case localBind l bind of
                  Right (Just graph) -> expr s (LM.insert (graphName graph) graph l) exp
                  Right Nothing -> expr s l exp
                  Left err -> error $ "Failed at translating local bind " ++ (showppr bind) ++ ". Error: " ++ err
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

varApp :: Maybe Signal -> LocalGraphs -> Var -> [C.CoreExpr] ->  GraphSE Signal
varApp s l v args
    | Just op <- binOp v
    , (typ, [opl, opr]) <- uncurry remove_predicates $ specialize (Var.varType v) args
    -- The application is a binary expression.
    = do let (argTypes, resultType) = splitFunTys typ
         lsig <- expr Nothing l opl
         rsig <- expr Nothing l opr
         bit <- ExceptT $ return $ getTypeBits resultType
         o <- lift $ autoSignal s bit
         lift $ insertNode $ BinNode o op lsig rsig
         return o
varApp s l v args
  | (t, args') <- specialize (Var.varType v) args
  , Just (l, r) <- Type.splitFunTy_maybe t
  = throwError $ "DEBUG: l=" ++ showppr l ++ ", ispred=" ++ (show $ Type.isPredTy l) ++ ", r=" ++ showppr r ++ ", ispred=" ++ (show $ Type.isPredTy r) ++ (showppr $ remove_predicates t args')

varApp _ _ v args = throwError $ "Unsupported varApp " ++ showppr v ++ " " ++ showppr args ++ " " ++ showppr t ++ " " ++ showppr (Type.splitFunTy_maybe t)
  where (t, _) = specialize (Var.varType v) args

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

bind :: C.CoreBind -> Either String (Maybe Graph)
bind (C.NonRec b e)
  |  "$trModule" <- getOccString b -- The module bind, ignore for now.
  = Right Nothing
bind b = localBind LM.empty b

localBind :: LocalGraphs -> C.CoreBind -> Either String (Maybe Graph)
localBind l (C.NonRec b e) =
  let (_, outputType) = splitFunTys $ varType b
      (decurriedExpr, inputVars) = decurry e []
      moduleName = getOccString b
      syn = do out <- ExceptT $ return $ mkSignal outputType "out"
               lift $ insertOutputSignal out
               expr (Just out) l decurriedExpr
      (ret, g) = runState (runExceptT syn) $ emptyGraph moduleName
  in case ret of
    Right _ -> Right (Just g)
    Left err -> Left err

localBind _ (C.Rec _) = Left "Cannot translate C.Rec"

decurry :: C.CoreExpr -> [Var] -> (C.CoreExpr, [Var])
decurry (C.Lam v e) as = (dexp, v:vs)
  where (dexp, vs) = decurry e as
decurry e as = (e, as)


mkVarSignal :: Var -> Either String Signal
mkVarSignal v = let n = Left $ getOccString $ Var.varName v
                in fmap (Signal n) $ getTypeBits (Var.varType v)

mkSignal :: Type -> String -> Either String Signal
mkSignal t n = fmap (Signal $ Left n) $ getTypeBits t

getTypeBits :: Type -> Either String Int
getTypeBits t | Just (tyCon, args) <- splitTyConApp_maybe t
              , isAlgTyCon tyCon
    = case getOccString $ getName tyCon of
        "Word" -> Right $ finiteBitSize (0::Word)
        "Bool" -> Right 1
        otherwise -> Left $ "Unsupported type " ++ showppr tyCon ++ (show $ fmap showppr $ tyConDataCons_maybe tyCon)
getTypeBits t = Left $ "Unsupported type " ++ showppr t

isVoidTy _ = False -- Obsolete in GHC 8.2. Finding replacement.
