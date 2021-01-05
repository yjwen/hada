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
import ListX (decap, decapAny)

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
  | Just (tname, fname) <- splitTypeFuncMaybe vname
  = getBuiltInExpr tname fname vis
  | otherwise
  = error $ "Unknown application " ++ vname
  where vname = getOccString $ varName v

getVExpr (Lam b exp) vis = getVExpr exp vis
getVExpr e vis = error ("Unexpected expression in getVExpr: " ++
                        (showSDocUnsafe $ myPprExpr e))

-- | Try to retrieve type and function from a string
-- Return Nothing if failed
splitTypeFuncMaybe :: String -> Maybe (String, String)
splitTypeFuncMaybe n
  | Just (_, tail) <- decapAny ["$fNum", "$fBits"] n
  -- Split a string of form like "$f[CNAME][TYPE]_$c[FUNC]", where
  --  [CNAME] must be either "Num" or "Bits", [TYPE] must be "Int",
  --  "Int8/16/32/34", "Word" or "Word8/16/32/64"
  = case decapAny ["Int", "Word" ] tail of
      Just (tname, tail1) ->
        case decapAny ["8", "16", "32", "64", ""] tail1 of
          Just (wname, tail2) ->
            case decap "_$c" tail2 of
              Just fname -> Just (tname ++ wname, fname)
              otherwise -> Nothing
          otherwise -> Nothing
      otherwise -> Nothing
  | Just (fname, tail) <- decapAny ["eq", "neq", "lt", "le", "gt", "ge"] n
  -- Split a string of form like "[FUNC][TYPE]", where [FUNC] must be
  -- one of the above list, and [TYPE] must be "Int", "Int8/16/32/64",
  -- "Word" or "Word8/16/32/64"
  = case decapAny ["Int", "Word"] tail of
      Just (tname, tail1) ->
        case elemIndex tail1 ["8", "16", "32", "64", ""] of
          Just _ -> Just (tail, fname)
          Nothing -> Nothing
      Nothing -> Nothing
  | otherwise
  = Nothing

getBuiltInExpr :: String -> String -> [Var] -> SDoc
getBuiltInExpr tname fname vis
  | fname == "+" = binaryExpr "+" vis
  | fname == "-" = binaryExpr "-" vis
  | fname == "*" = binaryExpr "*" vis
  | fname == "negate" = unaryExpr "-" vis
  | fname == "abs" = if "Int" `isPrefixOf` tname
                     then funCall ("hada::abs" ++ (autoWidthStr $ drop 3 tname)) vis
                     else varExpr vis
  | fname == "signum" = if "Int" `isPrefixOf` tname
                        then funCall ("hada::signum" ++ (autoWidthStr $ drop 3 tname)) vis
                        else funCall ("hada::signumU" ++ (autoWidthStr $ drop 4 tname)) vis
  | fname == "eq" = binaryExpr "==" vis
  | fname == "ne" = binaryExpr "!=" vis
  | fname == "lt" = binaryExpr "<" vis
  | fname == "le" = binaryExpr "<=" vis
  | fname == "gt" = binaryExpr ">" vis
  | fname == "ge" = binaryExpr ">=" vis
  | fname == ".&." = binaryExpr "&" vis
  | fname == ".|." = binaryExpr "|" vis
  | fname == "xor" = binaryExpr "^" vis
  | fname == "complement" = unaryExpr "~" vis
  | otherwise = error $ "Unknown builtin function " ++ fname
  where autoWidthStr str =  case str of
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
