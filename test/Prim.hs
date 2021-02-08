module Prim(plusOne, plusOne8, plusOne16, plusOne32, plusOne64,
            plusOneU, plusOne8U, plusOne16U, plusOne32U, plusOne64U) where
import Data.Int
import Data.Word

plusOne :: Int -> Int
plusOne = (+1)

plusOne8 :: Int8 -> Int8
plusOne8 = (+1)

plusOne16 :: Int16 -> Int16
plusOne16 = (+1)

plusOne32 :: Int32 -> Int32
plusOne32 = (+1)

plusOne64 :: Int64 -> Int64
plusOne64 = (+1)


plusOneU :: Word -> Word
plusOneU = (+1)

plusOne8U :: Word8 -> Word8
plusOne8U = (+1)

plusOne16U :: Word16 -> Word16
plusOne16U = (+1)

plusOne32U :: Word32 -> Word32
plusOne32U = (+1)

plusOne64U :: Word64 -> Word64
plusOne64U = (+1)
