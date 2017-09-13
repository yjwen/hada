module DFGSyn where
import Control.Monad.Trans.State
import qualified CoreSyn as C
import Outputable
import PprCore
import Data.List (intercalate)
import Data.Bits (shift, finiteBitSize)
import Literal
import Type
import TyCon
import Var
import Name (getOccString, getName)
import DataCon (dataConTag)
    
import DFG

showppr :: Outputable a => a -> String
showppr = showSDocUnsafe . ppr

expr :: C.CoreExpr -> Maybe Signal -> GraphS Signal
expr (C.Case e v t as) s =
    do cond <- expr e Nothing
       o <- autoSignal s $ getTypeBits t
       (dflt, branches) <- alts as
       case dflt of
         Just dflt' -> insertNode (CaseNode o cond dflt' branches)
         Nothing -> if length branches == (1 `shift` signalWidth cond)
                    then insertNode $ CaseNode o cond (snd $ head branches) (tail branches)
                    else error "Incomplete branches"
       return o

expr app@(C.App _ arg) s
    = case C.collectArgs app of
        (C.Var v, args) -> varApp v args s
        (C.Lam lv lexp, [C.Var larg]) ->
            if isVoidTy $ varType larg
            -- larg's type is void, the lambda
            -- is merely a wrapper of lexp,
            -- continue to synthesize lexp.
            then expr lexp s
            else error $ "Unsupported lambda application. " ++ (showppr app)
        _ -> error $ "Unsupported application. " ++ (showppr app)

expr (C.Var v) _ = insertSignal $ mkVarSignal v

expr (C.Lam v e) s =
    if isVoidTy $ varType v
    -- v's type is void, the lambda has no argument, continue to
    -- synthesis the lambda's expression.
    then expr e s
    else error $ "Unsupported lambda expression."

expr e _ = error $ "Unsupported Node type: " ++ showppr e


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
remove_predicates typ args
    | Just (appTyp, argType) <- Type.splitAppTy_maybe typ
    , Just (tycon, typs) <- Type.splitTyConApp_maybe appTyp
    , all isPredTy typs
    = (argType, drop (length typs) args) -- No type check. Simply drop those type-checking globals.
remove_predicates typ args = (typ, args)


varApp :: Var -> [C.CoreExpr] -> Maybe Signal -> GraphS Signal
varApp v args s
    | Just op <- binOp v
    , (typ, [opl, opr]) <- uncurry remove_predicates $ specialize (Var.varType v) args
    -- The application is a binary expression.
    = do let (argTypes, resultType) = splitFunTysN 2 typ
         lsig <- expr opl Nothing
         rsig <- expr opr Nothing
         o <- autoSignal s $ getTypeBits resultType
         insertNode $ BinNode o op lsig rsig
         return o

binOp :: Var -> Maybe BinOp
binOp var = case getOccString $ Var.varName var of
                "<" -> Just LessThan
                ">" -> Just GreaterThan
                "-" -> Just Minus
                otherwise -> Nothing


alts :: [C.Alt Var] -> GraphS (Maybe Signal, [(Integer, Signal)])
alts [] = do return (Nothing, [])
alts ((altCon, vars, exp):as) =
    do (dflt, branches) <- alts as
       sig <- expr exp Nothing
       case altCon of
         C.LitAlt lit -> case lit of
                           MachInt i -> return (dflt, (i, sig):branches)
                           _ -> error $ "Unsupported lit " ++ showppr lit
         C.DEFAULT -> return (Just sig, branches)
         C.DataAlt dc -> let tag = dataConTag dc
                         in return (dflt, (toInteger $ tag - 1, sig):branches)
translateBind :: C.CoreBind -> Maybe Graph
translateBind (C.NonRec b e) =
  let (_, outputType) = splitFunTys $ varType b
      (decurriedExpr, inputVars) = decurry e []
      moduleName = getOccString b
      out = mkSignal outputType "out"
      (_, g) = runState syn $ emptyGraph moduleName
      syn = do insertOutputSignal out
               expr decurriedExpr $ Just out
  in case head moduleName of
    '$' -> Nothing
    otherwise -> Just g

translateBind (C.Rec _) = error "Cannot translate C.Rec"

decurry :: C.CoreExpr -> [Var] -> (C.CoreExpr, [Var])
decurry (C.Lam v e) as = (dexp, v:vs)
  where (dexp, vs) = decurry e as
decurry e as = (e, as)


mkVarSignal :: Var -> Signal
mkVarSignal v = let n = Left $ getOccString $ Var.varName v
                in Signal n $ getTypeBits (Var.varType v)

mkSignal :: Type -> String -> Signal
mkSignal t n = Signal (Left n) $ getTypeBits t

getTypeBits :: Type -> Int
getTypeBits t | Just (tyCon, args) <- splitTyConApp_maybe t
              , isAlgTyCon tyCon
    = case getOccString $ getName tyCon of
        "Word" -> finiteBitSize (0::Word)
        "Bool" -> 1
        otherwise -> error $ "Unsupported type " ++ showppr tyCon
getTypeBits t = error $ "Unsupport type " ++ showppr t

