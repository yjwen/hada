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
           , instanceD (cxt []) (appT tFixedWidth tHidden)
             [ funP1D "fromFW" conName a [| extendSigned $(varE a) $(eBitWidth) |]
             , funP0D "toFW" $ conE conName
             ]
           , instanceD (cxt [appT tShow ta, appT tBits ta]) (appT tShow $ appT tHidden ta)
             [funS1D "show" a [| $(litE $ StringL typeStr) ++ " " ++ (show $ fromFW $(varE a))|]]
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
           , instanceD (cxt [appT tBits ta]) (appT tBits $ appT tHidden ta)
             [ funP0D ".&." [| \ a b -> toFW $ (fromFW a) .&. (fromFW b) |]
             , funP0D ".|." [| \ a b -> toFW $ (fromFW a) .|. (fromFW b) |]
             , funP0D "xor" [| \ a b -> toFW $ xor (fromFW a) (fromFW b) |]
             , funP0D "complement" [| toFW . complement . fromFW |]
             , funP0D "shiftL" [| \ a i -> toFW $ shiftL (fromFW a) i |]
             , funP0D "shiftR" [| \ a i -> toFW $ shiftR (fromFW a) i |]
             , funP0D "rotateL" [| \ a i -> toFW $ rotateLFW $(eBitWidth) (fromFW a) i |]
             , funP0D "rotateR" [| \ a i -> toFW $ rotateRFW $(eBitWidth) (fromFW a) i |]
             , funP0D "bitSize" [| \ a -> $(eBitWidth) |]
             , funP0D "bitSizeMaybe" [| \ a -> Just $(eBitWidth) |]
             , funP0D "isSigned" [| \ a -> True |]
             , funP0D "testBit" [| testBit . fromFW |]
             , funP0D "bit" [| toFW . bit |]
             , funP0D "popCount" [| (popCountFW $(eBitWidth)) . fromFW |]
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
           , instanceD (cxt [appT tBits ta]) (appT tBits $ appT tHidden ta)
             [ funP0D ".&." [| \ a b -> toFW $ (fromFW a) .&. (fromFW b) |]
             , funP0D ".|." [| \ a b -> toFW $ (fromFW a) .|. (fromFW b) |]
             , funP0D "xor" [| \ a b -> toFW $ xor (fromFW a) (fromFW b) |]
             , funP0D "complement" [| toFW . complement . fromFW |]
             , funP0D "shiftL" [| \ a i -> toFW $ shiftL (fromFW a) i |]
             , funP0D "shiftR" [| \ a i -> toFW $ shiftR (fromFW a) i |]
             , funP0D "rotateL" [| \ a i -> toFW $ rotateLFW $(eBitWidth) (fromFW a) i |]
             , funP0D "rotateR" [| \ a i -> toFW $ rotateRFW $(eBitWidth) (fromFW a) i |]
             , funP0D "bitSize" [| \ a -> $(eBitWidth) |]
             , funP0D "bitSizeMaybe" [| \ a -> Just $(eBitWidth) |]
             , funP0D "isSigned" [| \ a -> False |]
             , funP0D "testBit" [| testBit . fromFW |]
             , funP0D "bit" [| toFW . bit |]
             , funP0D "popCount" [| (popCountFW $(eBitWidth)) . fromFW |]
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

-- Function with one symbol, as: f (Con x) = expr
funS1D :: String -> Name -> ExpQ -> DecQ
funS1D funName x exp = funD (mkName funName) [clause [varP x] (normalB exp) []]


rotateLFW :: (Bits a) => Int -> a -> Int -> a
rotateLFW width a l = (shiftL a' l) .|. (shiftR a' (width - l))
  where a' = a .&. (complement $ shiftL (complement zeroBits) width)

rotateRFW :: (Bits a) => Int -> a -> Int -> a
rotateRFW width a r = (shiftR a' r) .|. (shiftL a' (width - r))
  where a' = a .&. (complement $ shiftL (complement zeroBits) width)

popCountFW :: (Bits a) => Int -> a -> Int
popCountFW width a = popCount $ a .&. mask
  where mask = complement $ shiftL (complement zeroBits) width
