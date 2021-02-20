module Prim( plusOne, plusOne8, plusOne16, plusOne32, plusOne64
           , plusOneU, plusOneU8, plusOneU16, plusOneU32, plusOneU64
           , lshc, lshc8, lshc16, lshc32, lshc64
           , rshc, rshc8, rshc16, rshc32, rshc64
           , lshcU, lshcU8, lshcU16, lshcU32, lshcU64
           , rshcU, rshcU8, rshcU16, rshcU32, rshcU64) where
import Data.Int
import Data.Word
import Data.Bits

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

plusOneU8 :: Word8 -> Word8
plusOneU8 = (+1)

plusOneU16 :: Word16 -> Word16
plusOneU16 = (+1)

plusOneU32 :: Word32 -> Word32
plusOneU32 = (+1)

plusOneU64 :: Word64 -> Word64
plusOneU64 = (+1)

-- Left shift by a constant
lshc :: Int -> Int
lshc = (`shift` 1)

lshc8 :: Int8 -> Int8
lshc8 = (`shift` 2)

lshc16 :: Int16 -> Int16
lshc16 = (`shift` 3)

lshc32 :: Int32 -> Int32
lshc32 = (`shift` 4)

lshc64 :: Int64 -> Int64
lshc64 = (`shift` 5)

lshcU :: Word -> Word
lshcU = (`shift` 1)

lshcU8 :: Word8 -> Word8
lshcU8 = (`shift` 2)

lshcU16 :: Word16 -> Word16
lshcU16 = (`shift` 3)

lshcU32 :: Word32 -> Word32
lshcU32 = (`shift` 4)

lshcU64 :: Word64 -> Word64
lshcU64 = (`shift` 5)

-- Right shift by a constant
rshc :: Int -> Int
rshc = (`shift` (-1))

rshc8 :: Int8 -> Int8
rshc8 = (`shift` (-1))

rshc16 :: Int16 -> Int16
rshc16 = (`shift` (-1))

rshc32 :: Int32 -> Int32
rshc32 = (`shift` (-1))

rshc64 :: Int64 -> Int64
rshc64 = (`shift` (-1))

rshcU :: Word -> Word
rshcU = (`shift` (-1))

rshcU8 :: Word8 -> Word8
rshcU8 = (`shift` (-1))

rshcU16 :: Word16 -> Word16
rshcU16 = (`shift` (-1))

rshcU32 :: Word32 -> Word32
rshcU32 = (`shift` (-1))

rshcU64 :: Word64 -> Word64
rshcU64 = (`shift` (-1))

