-- Type related synthesizing
module SynType (SynType, synType, showSV, showC, showHS) where
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

-- SV representations
appendSignSV :: Bool -> String -> String
appendSignSV True = id
appendSignSV False = (++ " unsigned")

showBitWidthSV :: BitWidth -> String
showBitWidthSV MachineInt = if (maxBound::Word) == 0xFFFFFFFF
                            then "int" -- 32 bit machine
                            else "longint"
showBitWidthSV SByte = "byte"
showBitWidthSV DByte = "shortint"
showBitWidthSV QByte = "int"
showBitWidthSV OByte = "longint"

showSV (SynInt bw s) = appendSignSV s $ showBitWidthSV bw
showSV SynBit = "bit"

appendSignC :: Bool -> String -> String
appendSignC True = id
appendSignC False = ('u':)

-- C representation
showBitWidthC :: BitWidth -> String
showBitWidthC MachineInt = if (maxBound::Word) == 0xFFFFFFFF
                           then "int32_t" -- 32 bit machine
                           else "int64_t"
showBitWidthC SByte = "int8_t"
showBitWidthC DByte = "int16_t"
showBitWidthC QByte = "int32_t"
showBitWidthC OByte = "int64_t"

showC :: SynType -> String
showC (SynInt bw s) = appendSignC s $ showBitWidthC bw
showC SynBit = "bool"

-- HS representation
showBitWidthHS :: BitWidth -> String
showBitWidthHS MachineInt = ""
showBitWidthHS SByte = "8"
showBitWidthHS DByte = "16"
showBitWidthHS QByte = "32"
showBitWidthHS OByte = "64"

showSignHS :: Bool -> String
showSignHS True = "Int"
showSignHS False = "Word"

showHS :: SynType -> String
showHS (SynInt bw s) = showSignHS s ++ showBitWidthHS bw
showHS SynBit = "Bool"
