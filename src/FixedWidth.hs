{-# LANGUAGE TemplateHaskell #-}
module FixedWidth where

import Language.Haskell.TH
import Data.Bits

newtypeFWD :: Name -> Name -> Name -> DecQ
newtypeFWD typeName conName baseTypeName =
  newtypeD -- newtype delcaration
  (cxt []) -- No type context
  typeName -- Make the type name
  [] -- No type variable binds
  (normalC conName [strictType notStrict $ conT baseTypeName]) -- Only one constructor, accepts an Int type
  [mkName "Show"] -- deriving

declareFWType :: Name -> Name -> DecQ
declareFWType typeName conName = newtypeFWD typeName conName $ mkName "Int"

declareUnsignedFWType :: Name -> Name -> DecQ
declareUnsignedFWType typeName conName = newtypeFWD typeName conName $ mkName "Word"

instanceHead :: String -> Name -> ([DecQ] -> DecQ)
instanceHead classStr typeName = instanceD (cxt []) (appT (conT $ mkName classStr) (conT typeName))

extendSigned :: Int -> Int -> Int
extendSigned v width = case (testBit v (width - 1)) of
                         True -> v .|. mask1
                         False -> v .&. mask0
  where mask0 = (bit width) - 1
        mask1 = complement mask0

extendUnsigned :: Word -> Int -> Word
extendUnsigned v width = v .&. (bit width - 1)
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

enumUnsignedInstance :: Name -> Name -> Int -> DecQ
enumUnsignedInstance typeName conName bitWidth = instanceHead "Enum" typeName [toEnumD, fromEnumD]
  where toEnumD = funP0D "toEnum" [| $(conE conName) . toEnum |]
        fromEnumD = funP1D "fromEnum" conName x [| fromEnum $ extendUnsigned $(varE x) bitWidth |]
          where x = mkName "x"
boundedInstance :: Name -> Name -> Int -> DecQ
boundedInstance typeName conName bitWidth = instanceHead "Bounded" typeName [minBoundD, maxBoundD]
  where minBoundD = funP0D "minBound" [| $(conE conName) (-(bit (bitWidth - 1))) |]
        maxBoundD = funP0D "maxBound" [| $(conE conName) ((bit (bitWidth - 1)) - 1)|]

boundedUnsignedD :: Name -> Name -> Int -> DecQ
boundedUnsignedD typeName conName bitWidth = instanceHead "Bounded" typeName [minBoundD, maxBoundD]
  where minBoundD = funP0D "minBound" [| $(conE conName) 0 |]
        maxBoundD = funP0D "maxBound" [| $(conE conName) $ (bit bitWidth - 1) |]
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

closedEnumFun2Tuple2 :: Enum a => (Int -> Int -> (Int, Int)) -> (a -> a -> (a, a))
closedEnumFun2Tuple2 f = \a b -> toTuple2 $ f (fromEnum a) (fromEnum b)
  where toTuple2 (x, y) = (toEnum x, toEnum y)

integralInstance :: Name -> DecQ
integralInstance typeName = instanceHead "Integral" typeName [quotD, remD, divD, modD, quotRemD, divModD, toIntegerD]
  where quotD = funP0D "quot" [| closedEnumFun2 quot |]
        remD = funP0D "rem" [| closedEnumFun2 rem |]
        divD = funP0D "div" [| closedEnumFun2 div |]
        modD = funP0D "mod" [| closedEnumFun2 mod |]
        quotRemD = funP0D "quotRem" [| closedEnumFun2Tuple2 quotRem |]
        divModD = funP0D "divMod" [| closedEnumFun2Tuple2 divMod |]
        toIntegerD = funP0D "toInteger" [| toInteger . fromEnum |]

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
     integralD <- integralInstance typeName
     return [typeD, enumD, boundedD, eqD, ordD, numD, realD, integralD]

declareUnsignedFW :: String -> String -> Int -> DecsQ
declareUnsignedFW typeStr conStr bitWidth =
  do let typeName = mkName typeStr
         conName = mkName conStr
     typeD <- declareUnsignedFWType typeName conName
     enumD <- enumUnsignedInstance typeName conName bitWidth
     boundedD <- boundedUnsignedD typeName conName bitWidth
     eqD <- eqInstance typeName
     ordD <- ordInstance typeName
     numD <- numInstance typeName
     return [typeD, enumD, boundedD, eqD, ordD, numD]
