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

extendSigned :: Int -> Int -> Int
extendSigned v width = case (testBit v (width - 1)) of
                         True -> v .|. mask1
                         False -> v .&. mask0
  where mask0 = (bit width) - 1
        mask1 = complement mask0

-- Helper functions for declaring functions
-- Function with zero argument, as minBound = expr
funP0D :: String -> ExpQ -> DecQ
funP0D funName exp = funD (mkName funName) [clause [] (normalB exp) []]

-- Function with one pattern, as: f (Con x) = expr
funP1D :: String -> Name -> Name -> ExpQ -> DecQ
funP1D funName con x exp = funD (mkName funName) [clause [conP con [varP x]] (normalB exp) []]
      
enumInstance :: Name -> Name -> Int -> DecQ
enumInstance typeName conName bitWidth = instanceHead "Enum" typeName [toEnumD, fromEnumD]
  where toEnumD = funP0D "toEnum" (conE conName)
        fromEnumD = funP1D "fromEnum" conName x [| extendSigned $(varE x) bitWidth |]
          where x = mkName "x"

boundedInstance :: Name -> Name -> Int -> DecQ
boundedInstance typeName conName bitWidth = instanceHead "Bounded" typeName [minBoundD, maxBoundD]
  where minBoundD = funP0D "minBound" [| $(conE conName) (-(bit (bitWidth - 1))) |]
        maxBoundD = funP0D "maxBound" [| $(conE conName) ((bit (bitWidth - 1)) - 1)|]


declareFW :: String -> String -> Int -> DecsQ
declareFW typeStr conStr bitWidth =
  do let typeName = mkName typeStr
         conName = mkName conStr
     typeD <- declareFWType typeName conName
     enumD <- enumInstance typeName conName bitWidth
     boundedD <- boundedInstance typeName conName bitWidth
     return [typeD, enumD, boundedD]
