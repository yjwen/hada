module Precedence( negSum, mulSum, sumSub, sumSum
                 , eqBand, bandXor, xorBor) where
import Data.Bits

negSum :: Int -> Int -> Int
negSum a b = -(a + b)

mulSum :: Int -> Int -> Int -> Int
mulSum a b c = a * (b + c)

sumSub :: Int -> Int -> Int -> Int
sumSub a b c = a + (b - c)

sumSum :: Int -> Int -> Int -> Int
sumSum a b c = a + b + c

eqBand :: Int -> Int -> Int -> Bool
eqBand a b c = a == (b .&. c)

bandXor :: Int -> Int -> Int -> Int
bandXor a b c = a .&. (b `xor` c)

xorBor :: Int -> Int -> Int -> Int
xorBor a b c = a `xor` (b .|. c)

-- andOr :: Int -> Int -> Int -> Bool
-- andOr a b c = ((a < b) || (a < c)) && ((a > b) || (a > c))
