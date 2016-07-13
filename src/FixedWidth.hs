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
  [mkName "Show"] -- deriving

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

enumFun2 :: Enum a => (Int -> Int -> b) -> (a -> a -> b)
enumFun2 f = \a b -> f (fromEnum a) (fromEnum b)

closedEnumFun2 :: Enum a => (Int -> Int -> Int) -> (a -> a -> a)
closedEnumFun2 f = \a b -> toEnum $ f (fromEnum a) (fromEnum b)

eqInstance :: Name -> DecQ
eqInstance typeName = instanceHead "Eq" typeName [eqD, neqD]
  where eqD = funP0D "==" [| enumFun2 (==) |]
        neqD = funP0D "/=" [| enumFun2 (/=) |]

ordInstance :: Name -> DecQ
ordInstance typeName = instanceHead "Ord" typeName [compareD]
  where compareD = funP0D "compare" [| enumFun2 compare |]

numInstance :: Name -> DecQ
numInstance typeName = instanceHead "Num" typeName [plusD, minusD, multD, absD, signumD, negateD, fromIntegerD]
  where plusD = funP0D "+" [| closedEnumFun2 (+) |]
        minusD = funP0D "-" [| closedEnumFun2 (-) |]
        multD = funP0D "*" [| closedEnumFun2 (*) |]
        absD = funP0D "abs" [| toEnum . abs . fromEnum |]
        signumD = funP0D "signum" [| toEnum . signum . fromEnum |]
        negateD = funP0D "negate" [| toEnum . negate . fromEnum |]
        fromIntegerD = funP0D "fromInteger" [| toEnum . fromInteger |]

realInstance :: Name -> DecQ
realInstance typeName = instanceHead "Real" typeName [toRationalD]
  where toRationalD = funP0D "toRational" [| toRational . fromEnum |]

declareFW :: String -> String -> Int -> DecsQ
declareFW typeStr conStr bitWidth =
  do let typeName = mkName typeStr
         conName = mkName conStr
     typeD <- declareFWType typeName conName
     enumD <- enumInstance typeName conName bitWidth
     boundedD <- boundedInstance typeName conName bitWidth
     eqD <- eqInstance typeName
     ordD <- ordInstance typeName
     numD <- numInstance typeName
     realD <- realInstance typeName
     return [typeD, enumD, boundedD, eqD, ordD, numD, realD]
