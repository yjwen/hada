-- For generating C++ driver file and the Haskell wrapper file for the
-- verilated Haskell bindings
module Verilator (cppDriver, hsWrapper) where
import Prelude hiding ((<>))
import Syn(collectInputVars)
import SynType(synType, cppr, hsppr)
import Util(nameIsInModule)
import CoreSyn
import Var
import Outputable
import Name(getName, getOccString, nameModule_maybe)
import Type(Type(..), splitTyConApp_maybe, tyConAppTyCon_maybe)
import TyCon(TyCon(..), tyConName)
import Module(Module(..))


-- | Generate the content of the C++ driver file for the verilated
-- function
cppDriver :: 
  CoreBind -> -- The top bind exported th Verilog and verilated
  Var -> -- The output variable exported to Verilog
  SDoc
cppDriver (NonRec b e) vo
  = let topName = text $ getOccString b
        vTopName = char 'V' <> topName
        inputVars = collectInputVars e
        oType = cppr $ synType $ varType vo
    in (text "#include <verilated.h>") $$
       (text "#include <cstdint>") $$
       (text "#include" <+> doubleQuotes (vTopName <> text ".h")) $$
       (indentBlock (text "extern \"C\"") (
           (indentBlock (oType <+> topName <+>
                         parens (vcat $
                                 punctuate (text ", ") $
                                 map
                                  (\v -> cppr (synType $ varType v) <+> cvar v)
                                  inputVars)) (
               -- The Body
               -- First, new the module instance
               (vTopName <+> text "*top = new" <+> vTopName <> semi) $$
               vcat (map
                      (\v -> text "top" <> text "->" <> cvar v <+> char '=' <+> cvar v <> semi)
                      inputVars) $$
               text "top->eval();" $$
               oType <+> cvar vo <+> char '=' <+> text "top->" <> cvar vo <> semi $$
               text "top->final();" $$
               text "delete top" <> semi $$
               (text "return" <+> cvar vo <> semi)
               ))))

cvar :: Var -> SDoc
cvar v = (text . getOccString . varName) v <> char '_' <> (ppr . varUnique) v

-- Write down the type in C++
ctype :: Type -> SDoc
ctype t
  | Just (tycon, tyapps) <- splitTyConApp_maybe t
  = ctypeTyCon tycon tyapps
  | otherwise
  = error $showSDocUnsafe (text "Unknown type at converting to C type:" <+> ppr t)

ctypeTyCon :: TyCon -> [Type] -> SDoc
ctypeTyCon con ts
  | nameIsInModule tn "GHC.Types" ||
    nameIsInModule tn "GHC.Int" ||
    nameIsInModule tn "GHC.Word"
  = ctypeBuiltin $ getOccString con
  where tn = tyConName con

ctypeBuiltin :: String -> SDoc
ctypeBuiltin n
  | n == "Int" = text (if (maxBound::Int) == 0x7FFFFFFF
                       then "int32_t"
                       else "int64_t")
  | n == "Word" = text (if (maxBound::Word) == 0xFFFFFFFF
                        then "uint32_t"
                        else "uint64_t")
  | n == "Int8" = text "int8_t"
  | n == "Int16" = text "int16_t"
  | n == "Int32" = text "int32_t"
  | n == "Int64" = text "int64_t"
  | n == "Word8" = text "uint8_t"
  | n == "Word16" = text "uint16_t"
  | n == "Word32" = text "uint32_t"
  | n == "Word64" = text "uint64_t"
  | otherwise = error ("Unknown builtin type " ++ n)
  
indentBlock :: SDoc -> SDoc -> SDoc
indentBlock head block = (head <+> lbrace) $+$ nest 2 block $+$ rbrace

-- | Content of the haskell wrapper for the C++ driver function
hsWrapper ::
  String -> -- The module name
  CoreBind -> -- The top bind exported th Verilog and verilated
  Var -> -- The output variable exported to Verilog
  SDoc
hsWrapper mName (NonRec b e) vo
  = let topName = text $ getOccString b
        inputVars = collectInputVars e
        inputSynTypes = map (synType . varType) inputVars
        typeModules = collectTypeModules $ map varType (vo:inputVars)
    in (text "{-# LANGUAGE ForeignFunctionInterface #-}") $$
       (text "module" <+> text mName <+> parens topName <+> text "where") $$
       (vcat $ map (\m -> text "import" <+> ppr m) typeModules) $$
       (text "foreign import ccall" <+> doubleQuotes topName <+>
        topName <+> dcolon <+>
        (hsep $ punctuate arrow $ map hsppr inputSynTypes) <+>
         arrow <+> text "IO" <+> hsppr (synType $ varType vo))
         
  
collectTypeModules :: [Type] -> [Module]
collectTypeModules ts = foldl addTypeModule [] ts
  where addTypeModule :: [Module] -> Type -> [Module]
        addTypeModule mods t = case tyConAppTyCon_maybe t of
                                 Just tyCon -> case nameModule_maybe $ getName tyCon of
                                                 Just m -> case elem m mods of
                                                             True -> mods
                                                             False -> (m:mods)
                                                 otherwise -> mods
                                 otherwise -> mods
