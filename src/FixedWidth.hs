module FixedWidth where

import Language.Haskell.TH

declareFWType :: String -> String -> DecQ
declareFWType typeName conName =
  dataD -- Data delcaration
  (cxt []) -- No type context
  (mkName typeName) -- Make the type name
  [] -- No type variable binds
  [normalC
   (mkName conName)
   [strictType isStrict $ conT (mkName "Int")]] -- Only one constructor, accepts an Int type
  [] -- No deriving
                                   
declareFW :: String -> String -> Int -> DecsQ
declareFW typeName conName bitWidth =
  do typeDec <- declareFWType typeName conName
     return [typeDec]
