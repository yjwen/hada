{-# LANGUAGE TemplateHaskell #-}
module FixedWidth where

import Language.Haskell.TH
import Data.Bits

declareFWType :: Name -> Name -> DecQ
declareFWType typeName conName =
  newtypeD -- newtype delcaration
  (cxt []) -- No type context
  typeName -- Make the type name
  [] -- No type variable binds
  (normalC conName [strictType notStrict $ conT (mkName "Int")]) -- Only one constructor, accepts an Int type
  [] -- No deriving

instanceHead :: String -> Name -> ([DecQ] -> DecQ)
instanceHead classStr typeName = instanceD (cxt []) (appT (conT $ mkName classStr) (conT typeName))

enumInstance :: Name -> Name -> Int -> DecQ
enumInstance typeName conName bitWidth = instanceHead "Enum" typeName [toEnumD, fromEnumD] where
  toEnumD = funD (mkName "toEnum") [clause [] (normalB $ conE $ conName) []]
  fromEnumD = funD (mkName "fromEnum") [clause [conP conName [varP x]] (normalB [| $(varE x) .&. ((bit bitWidth) - 1)|]) []] where x = mkName "x"



declareFW :: String -> String -> Int -> DecsQ
declareFW typeStr conStr bitWidth =
  do let typeName = mkName typeStr
         conName = mkName conStr
     typeD <- declareFWType typeName conName
     enumD <- enumInstance typeName conName bitWidth
     return [typeD, enumD]
