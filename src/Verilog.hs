module Verilog (toVerilog) where

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

toVerilog :: CoreBind -> IO SDoc
toVerilog (NonRec b e)
  | isGHCTypesTrNameSApp e = return $ empty
  | isGHCTypesModuleApp e = return $ empty
  | otherwise = toVModule b e

toVerilog (Rec bs) = error "Recursive bindings"

nameIsInModule :: String -> Name -> Bool
nameIsInModule s n = s == (moduleNameString . moduleName. nameModule) n


-- If the var is TrNameS defined in GHC.Types
isGHCTypesTrNameSApp :: CoreExpr -> Bool
isGHCTypesTrNameSApp (App (Var v) _) =
  (getOccString v == "TrNameS") && (nameIsInModule "GHC.Types" $ varName v)
isGHCTypesTrNameSApp _ = False

-- If the var is Module define in GHC.Types
isGHCTypesModuleApp :: CoreExpr -> Bool
isGHCTypesModuleApp (App (App (Var v) _) _) =
  (getOccString v == "Module") && (nameIsInModule "GHC.Types" $ varName v)
isGHCTypesModuleApp _ = False

toVModule :: CoreBndr -> CoreExpr -> IO SDoc
toVModule b e =
  do us <- mkSplitUniqSupply 'a'
     let (vo, vis) = initUs_ us $ getIOVars b e
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
  | nameIsInModule "GHC.Types" (tyConName con) -- Built-in types in GHC.Types
  = let conName = getOccString con
    in if conName == "Int"
       then
         if (maxBound::Int) == 0x7FFFFFFF
         -- Int is 32 bit
         then text "int"
         -- Assuming Int is 64 bit
         else text "longint"
       else ppr conName
  
getIOVars :: CoreBndr -> CoreExpr -> UniqSM (Var, [Var])
getIOVars b e = do let (iTypes, oType) = splitFunTys $ varType b;
                   vo <- mkOutputVar oType;
                   vis <- mkInputVars iTypes e
                   return (vo, vis)

newUniqueName :: FastString -> UniqSM Name
newUniqueName str = do u <- getUniqueM
                       return $ mkSystemVarName u str

mkAutoVar :: FastString -> Type -> UniqSM Var
mkAutoVar vname vtype = do n <- newUniqueName vname
                           return $ mkLocalVar VanillaId n vtype vanillaIdInfo
                           
mkOutputVar :: Type -> UniqSM Var
mkOutputVar = mkAutoVar $ mkFastString "o"

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
  | nameIsInModule "GHC.Num" $ varName v
  = getBuiltInExpr v vis
  | otherwise
  = ppr v

getBuiltInExpr :: Var -> [Var] -> SDoc
getBuiltInExpr v vis
  | vname == "$fNumInt_$c+"
  = binaryExpr "+" vis
  | vname == "$fNumInt_$c-"
  = binaryExpr "-" vis
  | otherwise
  = text "Unknown builtin"
  where vname = getOccString $ getName v

binaryExpr :: String -> [Var] -> SDoc
binaryExpr op (v0:v1:s) = ppr v0 <+> text op <+> ppr v1
binaryExpr _ _ = error "Insufficient argument for built-in binary expression"


                                   


