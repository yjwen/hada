{-# LANGUAGE TemplateHaskell #-}
module FixedWidth where

import Language.Haskell.TH
import Data.Bits

class FixedWidth f where
  fromFW :: Bits a => f a -> a
  toFW :: a -> f a

dataFWD :: Name -> Name -> DecQ
dataFWD typeName conName = do
  na <- newName "a" -- The type variable.
  dataD -- data declaration
    (cxt []) -- No context
    typeName
    [PlainTV na] -- Bit7Base a
    Nothing -- No Kind
    [normalC conName [bangType (bang noSourceUnpackedness noSourceStrictness) $ varT na]]
    (cxt []) -- No Deriving

tySynFWD :: Name -> Name -> Name -> DecQ
tySynFWD typeName hiddenTypeName baseTypeName =
  tySynD typeName [] $ appT (conT hiddenTypeName) (conT baseTypeName)

tShow = conT $ mkName "Show"
tFixedWidth = conT $ mkName "FixedWidth"
tEnum = conT $ mkName "Enum"
tBits = conT $ mkName "Bits"
tBounded = conT $ mkName "Bounded"
tEq = conT $ mkName "Eq"
tOrd = conT $ mkName "Ord"
tNum = conT $ mkName "Num"
tReal = conT $ mkName "Real"
tIntegral = conT $ mkName "Integral"

declareFW :: String -> String -> Int -> DecsQ
declareFW typeStr helperFunStr bitWidth = do
  let typeName = mkName typeStr
      helperFunName = mkName helperFunStr
      baseName = mkName "Int"
  conName <- newName typeStr
  a <- newName "a"
  hiddenTypeName <- newName $ typeStr ++ "__"
  let eBitWidth = litE $ IntegerL $ toInteger bitWidth
      tHidden = conT hiddenTypeName
      ta = varT a
  sequence [ dataFWD hiddenTypeName conName
           , tySynFWD typeName hiddenTypeName baseName
           , sigD helperFunName (appT (appT arrowT (conT baseName)) $ conT typeName)
           , funP0D helperFunStr (conE conName)
           , instanceD (cxt [appT tShow ta]) (appT tShow $ appT tHidden ta)
             [funP1D "show" conName a [| $(litE $ StringL typeStr) ++ " " ++ (show $(varE a))|]]
           , instanceD (cxt []) (appT tFixedWidth tHidden)
             [ funP1D "fromFW" conName a [| extendSigned $(varE a) $(eBitWidth) |]
             , funP0D "toFW" $ conE conName
             ]
           , instanceD (cxt [appT tEnum ta, appT tBits ta]) (appT tEnum $ appT tHidden ta)
             [ funP0D "fromEnum" [| fromEnum . fromFW |]
             , funP0D "toEnum" [| toFW . toEnum |]
             ]
           , instanceD (cxt [appT tBounded ta, appT tBits ta]) (appT tBounded $ appT tHidden ta)
             [ funP0D "minBound" [| toFW $ bit ($(eBitWidth) - 1) |]
             , funP0D "maxBound" [| toFW $ complement $ shiftL (complement zeroBits) ($(eBitWidth) - 1) |]
             ]
           , instanceD (cxt [appT tEq ta, appT tBits ta]) (appT tEq $ appT tHidden ta)
             [ funP0D "==" [| \ a b -> (fromFW a) == (fromFW b) |]
             , funP0D "/=" [| \ a b -> (fromFW a) /= (fromFW b) |]
             ]
           , instanceD (cxt [appT tOrd ta, appT tBits ta]) (appT tOrd $ appT tHidden ta)
             [funP0D "compare" [| \ a b -> compare (fromFW a) (fromFW b) |]]
           , instanceD (cxt [appT tNum ta, appT tBits ta]) (appT tNum $ appT tHidden ta)
             [ funP0D "+" [| \ a b -> toFW $ (fromFW a) + (fromFW b) |]
             , funP0D "-" [| \ a b -> toFW $ (fromFW a) - (fromFW b) |]
             , funP0D "*" [| \ a b -> toFW $ (fromFW a) * (fromFW b) |]
             , funP0D "abs" [| toFW . abs . fromFW |]
             , funP0D "signum" [| toFW . signum . fromFW |]
             , funP0D "negate" [| toFW . negate . fromFW |]
             , funP0D "fromInteger" [| toFW . fromInteger |]
             ]
           , instanceD (cxt [appT tReal ta, appT tBits ta]) (appT tReal $ appT tHidden ta)
             [funP0D "toRational" [| toRational . fromFW |]]
           , instanceD (cxt [appT tIntegral ta, appT tBits ta]) (appT tIntegral $ appT tHidden ta)
             [ funP0D "quot" [| \ a b -> toFW $ quot (fromFW a) (fromFW b) |]
             , funP0D "rem"  [| \ a b -> toFW $ rem (fromFW a) (fromFW b) |]
             , funP0D "div"  [| \ a b -> toFW $ div (fromFW a) (fromFW b) |]
             , funP0D "mod"  [| \ a b -> toFW $ mod (fromFW a) (fromFW b) |]
             , funP0D "quotRem" [| \ a b -> let (x, y) = quotRem (fromFW a) (fromFW b) in (toFW x, toFW y) |]
             , funP0D "divMod" [| \ a b -> let (x, y) = divMod (fromFW a) (fromFW b) in (toFW x, toFW y) |]
             , funP0D "toInteger" [| toInteger .fromFW |]
             ]
           ]

declareUFW :: String -> String -> Int -> DecsQ
declareUFW typeStr helperFunStr bitWidth = do
  let typeName = mkName typeStr
      helperFunName = mkName helperFunStr
      baseName = mkName "Word"
  conName <- newName typeStr
  a <- newName "a"
  hiddenTypeName <- newName $ typeStr ++ "__"
  let eBitWidth = litE $ IntegerL $ toInteger bitWidth
      tHidden = conT hiddenTypeName
      ta = varT a
  sequence [ dataFWD hiddenTypeName conName
           , tySynFWD typeName hiddenTypeName baseName
           , sigD helperFunName (appT (appT arrowT (conT baseName)) $ conT typeName)
           , funP0D helperFunStr (conE conName)
           , instanceD (cxt [appT tShow ta]) (appT tShow $ appT tHidden ta)
             [funP1D "show" conName a [| $(litE $ StringL typeStr) ++ " " ++ (show $(varE a))|]]
           , instanceD (cxt []) (appT tFixedWidth tHidden)
             [ funP1D "fromFW" conName a [| extendUnsigned $(varE a) $(eBitWidth)|]
             , funP0D "toFW" $ conE conName
             ]
           , instanceD (cxt [appT tEnum ta, appT tBits ta]) (appT tEnum $ appT tHidden ta)
             [ funP0D "fromEnum" [| fromEnum . fromFW |]
             , funP0D "toEnum" [| toFW . toEnum |]
             ]
           , instanceD (cxt [appT tBounded ta, appT tBits ta]) (appT tBounded $ appT tHidden ta)
             [ funP0D "minBound" [| toFW zeroBits |]
             , funP0D "maxBound" [| toFW $ complement $ shiftL (complement zeroBits) $(eBitWidth) |]
             ]
           , instanceD (cxt [appT tEq ta, appT tBits ta]) (appT tEq $ appT tHidden ta)
             [ funP0D "==" [| \ a b -> (fromFW a) == (fromFW b)|]
             , funP0D "/=" [| \ a b -> (fromFW a) /= (fromFW b)|]
             ]
           , instanceD (cxt [appT tOrd ta, appT tBits ta]) (appT tOrd $ appT tHidden ta)
             [funP0D "compare" [| \ a b -> compare (fromFW a) (fromFW b) |]]
           , instanceD (cxt [appT tNum ta, appT tBits ta]) (appT tNum $ appT tHidden ta)
             [ funP0D "+" [| \ a b -> toFW $ (fromFW a) + (fromFW b) |]
             , funP0D "-" [| \ a b -> toFW $ (fromFW a) - (fromFW b) |]
             , funP0D "*" [| \ a b -> toFW $ (fromFW a) * (fromFW b) |]
             , funP0D "abs" [| toFW . abs . fromFW |]
             , funP0D "signum" [| toFW . signum . fromFW |]
             , funP0D "negate" [| toFW . negate . fromFW |]
             , funP0D "fromInteger" [| toFW . fromInteger |]
             ]
           , instanceD (cxt [appT tReal ta, appT tBits ta]) (appT tReal $ appT tHidden ta)
             [funP0D "toRational" [| toRational . fromFW |]]
           , instanceD (cxt [appT tIntegral ta, appT tBits ta]) (appT tIntegral $ appT tHidden ta)
             [ funP0D "quot" [| \ a b -> toFW $ quot (fromFW a) (fromFW b) |]
             , funP0D "rem"  [| \ a b -> toFW $ rem (fromFW a) (fromFW b) |]
             , funP0D "div"  [| \ a b -> toFW $ div (fromFW a) (fromFW b) |]
             , funP0D "mod"  [| \ a b -> toFW $ mod (fromFW a) (fromFW b) |]
             , funP0D "quotRem" [| \ a b -> let (x, y) = quotRem (fromFW a) (fromFW b) in (toFW x, toFW y) |]
             , funP0D "divMod" [| \ a b -> let (x, y) = divMod (fromFW a) (fromFW b) in (toFW x, toFW y) |]
             , funP0D "toInteger" [| toInteger .fromFW |]
             ]
           ]
  
extendSigned :: (Bits a) => a -> Int -> a
extendSigned v width = case (testBit v (width - 1)) of
                         True -> v .|. mask1
                         False -> v .&. mask0
  where mask0 = complement mask1
        mask1 = shiftL (complement zeroBits) width

extendUnsigned :: (Bits a) => a -> Int -> a
extendUnsigned v width = v .&. (complement $ shiftL (complement zeroBits) width)

-- Helper functions for declaring functions
-- Function with zero argument, as minBound = expr
funP0D :: String -> ExpQ -> DecQ
funP0D funName exp = funD (mkName funName) [clause [] (normalB exp) []]

-- Function with one pattern, as: f (Con x) = expr
funP1D :: String -> Name -> Name -> ExpQ -> DecQ
funP1D funName con x exp = funD (mkName funName) [clause [conP con [varP x]] (normalB exp) []]
      























