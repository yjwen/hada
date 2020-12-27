module SVerilog (toSV) where

import Prelude hiding ((<>))
import MyPpr
import Syn (collectInputVars)
import SynType (synType)
import Util (varIsInModule)
import Outputable
import CoreSyn
import Name (Name, nameModule, getOccString, getName, mkSystemVarName)
import Module (moduleName, moduleNameString)
import Var (varName, varType, Var, mkLocalVar)
import Type (Type(..), splitFunTys, getTyVar)
import TyCon
import UniqSupply (UniqSM, initUs_, mkSplitUniqSupply, getUniqueM)
import IdInfo (IdDetails(VanillaId), vanillaIdInfo)
import FastString (FastString, mkFastString)

import Data.List

import MyPpr -- For dumping

toSV :: CoreBind -> IO (SDoc, Var)
toSV (NonRec b e) = toVModule b e
toSV (Rec bs) = error "Recursive bindings"

-- Return the translated Verilog SDoc, and the created output var for
-- wrapper files
toVModule :: CoreBndr -> CoreExpr -> IO (SDoc, Var)
toVModule b e =
  do us <- mkSplitUniqSupply 'a'
     let (_, otype) = splitFunTys $ varType b
         vo = initUs_ us $ mkAutoVar (mkFastString "o") otype
         vis = collectInputVars e
     return ((text "module" <+> ppr b <+>
              -- Port definition
              parens (vcat $ punctuate (text ", ")  ((outputDef vo):(map inputDef vis))) <> semi
              $+$
              -- Body
              nest 2 (getStatement vo e vis)
              $+$
              text "endmodule")
            , vo)

-- | Generate the output port definition
outputDef :: Var -> SDoc
outputDef v = text "output" <+> (ppr . synType . varType)  v <+> (ppr v)

-- | Generate the input port definition
inputDef :: Var -> SDoc
inputDef v = text "input" <+> (ppr . synType . varType) v <+> (ppr v)


newUniqueName :: FastString -> UniqSM Name
newUniqueName str = do u <- getUniqueM
                       return $ mkSystemVarName u str

mkAutoVar :: FastString -> Type -> UniqSM Var
mkAutoVar vname vtype = do n <- newUniqueName vname
                           return $ mkLocalVar VanillaId n vtype vanillaIdInfo


-- | Get those vars presented in the expression and argument
getExplicitInputVars :: CoreExpr -> [Var] -> [Var]
getExplicitInputVars (App e (Var a)) args = a:args
getExplicitInputVars (App e _) args = error "Unkown argument in getExplicitInputVars"
-- | Assuming v is an synthesizable expression
getExplicitInputVars (Var v) args = args
getExplicitInputVars e _ = error "Unexpected expression in getExplicitInputVars: "

-- | Trying to convert an core expression to a variable. Expecting the
-- expression itself being a (Var v) or (Type t)
getExprVar :: CoreExpr -> Var
getExprVar (Var v) = v
getExprVar (Type t) = getTyVar "Cannot get TyVar" t


getStatement :: Var -> CoreExpr -> [Var] -> SDoc
getStatement vo e vis =
  -- Assuming the binder can always be implemented by combinational
  -- logic
  text "always_comb" <+> ppr vo <+> text "=" <+> getVExpr e vis <> semi

getVExpr :: CoreExpr -> [Var] -> SDoc
getVExpr (App e args) vis = getVExpr e vis
getVExpr (Var v) vis
  | varIsInModule v "GHC.Num" ||
    varIsInModule v "GHC.Int" ||
    varIsInModule v "GHC.Word"
  = getBuiltInExpr v vis
  | otherwise
  = ppr v
getVExpr (Lam b exp) vis = getVExpr exp vis
getVExpr e vis = error ("Unexpected expression in getVExpr: " ++
                        (showSDocUnsafe $ myPprExpr e))
                    
getBuiltInExpr :: Var -> [Var] -> SDoc
getBuiltInExpr v vis
  | fname == "$c+" && isNum
  = binaryExpr "+" vis
  | fname == "$c-" && isNum
  = binaryExpr "-" vis
  | fname == "$c*" && isNum
  = binaryExpr "*" vis
  | fname == "$cnegate" && isNum
  = unaryExpr "-" vis
  | fname == "$cabs" && isNum
  = if "$fNumInt" `isPrefixOf` cname
    -- Int, Int8 ~ Int64
    then funCall ("hada::abs" ++ (autoWidthStr $ drop 8 cname)) vis
    -- Word, Word8 ~ Word64
    else varExpr vis
  | fname == "$csignum" && isNum
  = if "$fNumInt" `isPrefixOf` cname
    -- Int, Int8 ~ Int64
    then funCall ("hada::signum" ++ (autoWidthStr $ drop 8 cname)) vis
    -- Word, Word8 ~ Word64
    else funCall ("hada::signumU" ++ (autoWidthStr $ drop 9 cname)) vis
  | otherwise
  = text "Unknown builtin"
  where vname = getOccString $ getName v
        (cname, fname') = break (== '_') vname
        fname = drop 1 fname'
        isNum = ("$fNumInt" `isPrefixOf` cname) || ("$fNumWord" `isPrefixOf` cname)
        autoWidthStr str =  case str of
                              [] -> if (maxBound::Word) == 0xFFFFFFFF
                                    then "32"
                                    else "64"
                              otherwise -> str

binaryExpr :: String -> [Var] -> SDoc
binaryExpr op (v0:v1:s) = ppr v0 <+> text op <+> ppr v1
binaryExpr _ _ = error "Insufficient operands for built-in binary expression"

unaryExpr :: String -> [Var] -> SDoc
unaryExpr op vs = text op <> varExpr vs

varExpr :: [Var] -> SDoc
varExpr (v:vs) = ppr v
varExpr _ = error "Insufficient operand" 
                                   
funCall :: String -> [Var] -> SDoc
funCall fName vs = text fName <> parens (pprWithCommas ppr vs)
