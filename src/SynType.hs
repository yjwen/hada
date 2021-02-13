-- Type related synthesizing
module SynType (SynType, synType, cppr, hsppr) where
import Prelude hiding ((<>))
import NameX(isInModule)
import Type (Type, splitTyConApp_maybe)
import TyCon (tyConName)
import Name (getOccString, nameModule)
import Module (moduleNameString, moduleName)
import TyCon (TyCon)
import Outputable

data BitWidth = MachineInt | SByte | DByte | QByte | OByte
              deriving (Show)
-- Synthesizable type
data SynType = SynInt { bitWidth :: BitWidth
                      , isSigned :: Bool }
             | SynBit
             deriving (Show)

-- Try to synthesize a type
synType :: Type -> SynType
synType t
  | Just (tycon, tyapps) <- splitTyConApp_maybe t
  = synTyConApp tycon tyapps
  | otherwise
  = error $ showSDocUnsafe (text "Unknown type for type declaration:" <+> ppr t)

-- Synthesize a type constructor application
synTyConApp :: TyCon -> [Type] -> SynType
synTyConApp con ts
  | isInModule con "GHC.Types" || -- Built-in types in GHC.Types
    isInModule con "GHC.Int" ||
    isInModule con "GHC.Word" ||
    isInModule con "GHC.Prim"
  = builtinType $ getOccString con

synTyConApp con ts
  = error ("Unknown type: " ++ (moduleNameString $ moduleName $ nameModule $ tyConName con))


builtinType :: String -> SynType
builtinType n
  | n == "Int" = SynInt MachineInt True
  | n == "Int8" = SynInt SByte True
  | n == "Int16" = SynInt DByte True
  | n == "Int32" = SynInt QByte True
  | n == "Int64" = SynInt OByte True
  | n == "Word" = SynInt MachineInt False
  | n == "Word8" = SynInt SByte False
  | n == "Word16" = SynInt DByte False
  | n == "Word32" = SynInt QByte False
  | n == "Word64" = SynInt OByte False
  | n == "Bool" = SynBit
  | n == "Int#" = SynInt MachineInt True
  | n == "Word#" = SynInt MachineInt False
  | otherwise = error ("Unknown builtin type: " ++ n)

signess True = empty
signess False = text "unsigned"

instance Outputable SynType where
  ppr (SynInt MachineInt s) = text (if (maxBound::Word) == 0xFFFFFFFF
                                     -- 32 bit machine
                                     then "int"
                                     else "longint") <+> signess s
  ppr (SynInt SByte s) = text "byte" <+> signess s
  ppr (SynInt DByte s) = text "shortint" <+> signess s
  ppr (SynInt QByte s) = text "int" <+> signess s
  ppr (SynInt OByte s) = text "longint" <+> signess s
  ppr SynBit = text "bit"

csigness True = empty
csigness False = char 'u'

-- For ppr syn types as C types
cppr :: SynType -> SDoc
cppr (SynInt MachineInt s) = csigness s <> text (if (maxBound::Word) == 0xFFFFFFFF
                                                  -- 32 bit machine
                                                  then "int32_t"
                                                  else "int64_t")
cppr (SynInt SByte s) = csigness s <> text "int8_t"
cppr (SynInt DByte s) = csigness s <> text "int16_t"
cppr (SynInt QByte s) = csigness s <> text "int32_t"
cppr (SynInt OByte s) = csigness s <> text "int64_t"
cppr SynBit = text "bool"

-- For ppr syn types back as Haskell types
hsppr :: SynType -> SDoc
hsppr (SynInt MachineInt s) = text $ case s of True -> "Int"
                                               False -> "Word"
hsppr (SynInt SByte s) = text $ case s of True -> "Int8"
                                          False -> "Word8"
hsppr (SynInt DByte s) = text $ case s of True -> "Int16"
                                          False -> "Word16"
hsppr (SynInt QByte s) = text $ case s of True -> "Int32"
                                          False -> "Word32"
hsppr (SynInt OByte s) = text $ case s of True -> "Int64"
                                          False -> "Word64"
hsppr SynBit = text "Bool"
