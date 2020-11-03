module SVerilog (toSV) where

import MyPpr
import Syn (nameIsInModule)
import Outputable
import CoreSyn
import Name (Name, nameModule, getOccString, getName, mkSystemVarName)
import Module (moduleName, moduleNameString)
import Var (varName, varType, Var, mkLocalVar)
import Type (Type(..), splitFunTys, splitTyConApp_maybe, getTyVar)
import TyCon
import UniqSupply (UniqSM, initUs_, mkSplitUniqSupply, getUniqueM)
import IdInfo (IdDetails(VanillaId), vanillaIdInfo)
import FastString (FastString, mkFastString)

import MyPpr -- For dumping

toSV :: CoreBind -> IO SDoc
toSV (NonRec b e) = toVModule b e
toSV (Rec bs) = error "Recursive bindings"

toVModule :: CoreBndr -> CoreExpr -> IO SDoc
toVModule b e =
  do us <- mkSplitUniqSupply 'a'
     let (_, otype) = splitFunTys $ varType b
         vo = initUs_ us $ mkAutoVar (mkFastString "o") otype
         vis = collectInputVars e
     return $ (text "module" <+> ppr b <+>
               -- Port definition
               parens (vcat $ punctuate (text ", ")  ((outputDef vo):(map inputDef vis))) <> semi
               $+$
               -- Body
               nest 2 (getStatement vo e vis)
               $+$
               text "endmodule")

-- | Generate the output port definition
outputDef :: Var -> SDoc
outputDef v = text "output" <+> (typeDeclaration $ varType v) <+> (ppr v)

-- | Generate the input port definition
inputDef :: Var -> SDoc
inputDef v = text "input" <+> (typeDeclaration $ varType v) <+> (ppr v)

-- How to represent the type in a input/output declaration
typeDeclaration :: Type -> SDoc
typeDeclaration t
  | Just (tycon, tyapps) <- splitTyConApp_maybe t
  = tyConDeclaration tycon tyapps
  | otherwise
  = error $ showSDocUnsafe (text "Unknown type for type declaration:" <+> ppr t)

-- How to represent a TyCon application in a input/output declaration
tyConDeclaration :: TyCon -> [Type] -> SDoc
tyConDeclaration con ts
  | nameIsInModule "GHC.Types" tn || -- Built-in types in GHC.Types
    nameIsInModule "GHC.Int" tn ||
    nameIsInModule "GHC.Word" tn
  = builtInTypeCon $ getOccString con
  where tn = tyConName con

tyConDeclaration con ts
  = error ("Unknown type: " ++ (moduleNameString $ moduleName $ nameModule $ tyConName con))


builtInTypeCon :: String -> SDoc
builtInTypeCon n
  | n == "Int" = if (maxBound::Int) == 0x7FFFFFFF
                    -- Int is 32 bit
                 then text "int"
                      -- Assuming Int is 64 bit
                 else text "longint"
  | n == "Word" = if (maxBound::Word) == 0xFFFFFFFF
                     -- Word is 32 bit
                  then text "int unsigned"
                  else text "longint unsigned"
  | n == "Int8" = text "byte"
  | n == "Int16" = text "shortint"
  | n == "Int32" = text "int"
  | n == "Int64" = text "longint"
  | n == "Word8" = text "byte unsigned"
  | n == "Word16" = text "shortint unsigned"
  | n == "Word32" = text "int unsigned"
  | n == "Word64" = text "longint unsigned"
  | otherwise = ppr n
  

newUniqueName :: FastString -> UniqSM Name
newUniqueName str = do u <- getUniqueM
                       return $ mkSystemVarName u str

mkAutoVar :: FastString -> Type -> UniqSM Var
mkAutoVar vname vtype = do n <- newUniqueName vname
                           return $ mkLocalVar VanillaId n vtype vanillaIdInfo

-- | Assuming the expression being η-abstracted.
collectInputVars :: CoreExpr -> [Var]
collectInputVars (Lam b e) = b:collectInputVars e
collectInputVars _ = []

-- | Extract input vars from the expression and arg. Create unique vars
-- to represent input if necessary
mkInputVars :: [Type] -> CoreExpr -> UniqSM [Var]
mkInputVars iTypes e = do let explictVars = getExplicitInputVars e []
                              autoTypes = drop (length explictVars) iTypes
                          autoVars <- mapM (mkAutoVar $ mkFastString "i") autoTypes
                          return $ explictVars ++ autoVars

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
  | (nameIsInModule "GHC.Num" vn) ||
    (nameIsInModule "GHC.Int" vn) ||
    (nameIsInModule "GHC.Word" vn)
  = getBuiltInExpr v vis
  | otherwise
  = ppr v
  where vn = varName v
getVExpr (Lam b exp) vis = getVExpr exp vis
getVExpr e vis = error ("Unexpected expression in getVExpr: " ++
                        (showSDocUnsafe $ myPprExpr e))
                    
getBuiltInExpr :: Var -> [Var] -> SDoc
getBuiltInExpr v vis
  | vname == "$fNumInt_$c+" ||
    vname == "$fNumWord_$c+" ||
    vname == "$fNumInt8_$c+" ||
    vname == "$fNumWord8_$c+" ||
    vname == "$fNumInt16_$c+" ||
    vname == "$fNumWord16_$c+" ||
    vname == "$fNumInt32_$c+" ||
    vname == "$fNumWord32_$c+" ||
    vname == "$fNumInt64_$c+" ||
    vname == "$fNumWord64_$c+"
  = binaryExpr "+" vis
  | vname == "$fNumInt_$c-" ||
    vname == "$fNumWord_$c-" ||
    vname == "$fNumInt8_$c-" ||
    vname == "$fNumWord8_$c-" ||
    vname == "$fNumInt16_$c-" ||
    vname == "$fNumWord16_$c-" ||
    vname == "$fNumInt32_$c-" ||
    vname == "$fNumWord32_$c-" ||
    vname == "$fNumInt64_$c-" ||
    vname == "$fNumWord64_$c-"
  = binaryExpr "-" vis
  | vname == "$fNumInt_$c*" ||
    vname == "$fNumWord_$c*" ||
    vname == "$fNumInt8_$c*" ||
    vname == "$fNumWord8_$c*" ||
    vname == "$fNumInt16_$c*" ||
    vname == "$fNumWord16_$c*" ||
    vname == "$fNumInt32_$c*" ||
    vname == "$fNumWord32_$c*" ||
    vname == "$fNumInt64_$c*" ||
    vname == "$fNumWord64_$c*"
  = binaryExpr "*" vis
  | otherwise
  = text "Unknown builtin"
  where vname = getOccString $ getName v

binaryExpr :: String -> [Var] -> SDoc
binaryExpr op (v0:v1:s) = ppr v0 <+> text op <+> ppr v1
binaryExpr _ _ = error "Insufficient argument for built-in binary expression"


                                   


