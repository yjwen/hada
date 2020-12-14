module Num(plus, minus, mul, neg,
           plusW, minusW, mulW, negW,
           plus8, minus8, mul8, neg8,
           plusW8, minusW8, mulW8, negW8,
           plus16, minus16, mul16, neg16,
           plusW16, minusW16, mulW16, negW16,
           plus32, minus32, mul32, neg32,
           plusW32, minusW32, mulW32, negW32,
           plus64, minus64, mul64, neg64,
           plusW64, minusW64, mulW64, negW64) where

import Data.Int
import Data.Word

plus :: Int -> Int -> Int
plus = (+)

minus :: Int -> Int -> Int
minus = (-)

mul :: Int -> Int -> Int
mul = (*)

neg :: Int -> Int
neg = negate

plusW :: Word -> Word -> Word
plusW = (+)

minusW :: Word -> Word -> Word
minusW = (-)

mulW :: Word -> Word -> Word
mulW = (*)

negW :: Word -> Word
negW = negate

plus8 :: Int8 -> Int8 -> Int8
plus8 = (+)

minus8 :: Int8 -> Int8 -> Int8
minus8 = (-)

mul8 :: Int8 -> Int8 -> Int8
mul8 = (*)

neg8 :: Int8 -> Int8
neg8 = negate

plusW8 :: Word8 -> Word8 -> Word8
plusW8 = (+)

minusW8 :: Word8 -> Word8 -> Word8
minusW8 = (-)

mulW8 :: Word8 -> Word8 -> Word8
mulW8 = (*)

negW8 :: Word8 -> Word8
negW8 = negate

plus16 :: Int16 -> Int16 -> Int16
plus16 = (+)

minus16 :: Int16 -> Int16 -> Int16
minus16 = (-)

mul16 :: Int16 -> Int16 -> Int16
mul16 = (*)

neg16 :: Int16 -> Int16
neg16 = negate

plusW16 :: Word16 -> Word16 -> Word16
plusW16 = (+)

minusW16 :: Word16 -> Word16 -> Word16
minusW16 = (-)

mulW16 :: Word16 -> Word16 -> Word16
mulW16 = (*)

negW16 :: Word16 -> Word16
negW16 = negate

plus32 :: Int32 -> Int32 -> Int32
plus32 = (+)

minus32 :: Int32 -> Int32 -> Int32
minus32 = (-)

mul32 :: Int32 -> Int32 -> Int32
mul32 = (*)

neg32 :: Int32 -> Int32
neg32 = negate

plusW32 :: Word32 -> Word32 -> Word32
plusW32 = (+)

minusW32 :: Word32 -> Word32 -> Word32
minusW32 = (-)

mulW32 :: Word32 -> Word32 -> Word32
mulW32 = (*)

negW32 :: Word32 -> Word32
negW32 = negate

plus64 :: Int64 -> Int64 -> Int64
plus64 = (+)

minus64 :: Int64 -> Int64 -> Int64
minus64 = (-)

mul64 :: Int64 -> Int64 -> Int64
mul64 = (*)

neg64 :: Int64 -> Int64
neg64 = negate

plusW64 :: Word64 -> Word64 -> Word64
plusW64 = (+)

minusW64 :: Word64 -> Word64 -> Word64
minusW64 = (-)

mulW64 :: Word64 -> Word64 -> Word64
mulW64 = (*)

negW64 :: Word64 -> Word64
negW64 = negate
