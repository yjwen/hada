module TypeX (isBoolType) where

import Type
import NameX
import Name

isBoolType :: Type -> Bool
isBoolType t
  | Just tycon <- tyConAppTyCon_maybe t
  = getOccString tycon == "Bool" && (tycon `isInModule` "GHC.Types")
  | otherwise
  = False
  
