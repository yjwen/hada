module Num(plus, minus, mul,
           plusW, minusW, mulW,
           plus8, minus8, mul8,
           plusW8, minusW8, mulW8,
           plus16, minus16, mul16,
           plusW16, minusW16, mulW16,
           plus32, minus32, mul32,
           plusW32, minusW32, mulW32,
           plus64, minus64, mul64,
           plusW64, minusW64, mulW64) where

import Data.Int
import Data.Word

plus :: Int -> Int -> Int
plus = (+)

minus :: Int -> Int -> Int
minus = (-)

mul :: Int -> Int -> Int
mul = (*)

plusW :: Word -> Word -> Word
plusW = (+)

minusW :: Word -> Word -> Word
minusW = (-)

mulW :: Word -> Word -> Word
mulW = (*)

plus8 :: Int8 -> Int8 -> Int8
plus8 = (+)

minus8 :: Int8 -> Int8 -> Int8
minus8 = (-)

mul8 :: Int8 -> Int8 -> Int8
mul8 = (*)

plusW8 :: Word8 -> Word8 -> Word8
plusW8 = (+)

minusW8 :: Word8 -> Word8 -> Word8
minusW8 = (-)

mulW8 :: Word8 -> Word8 -> Word8
mulW8 = (*)

plus16 :: Int16 -> Int16 -> Int16
plus16 = (+)

minus16 :: Int16 -> Int16 -> Int16
minus16 = (-)

mul16 :: Int16 -> Int16 -> Int16
mul16 = (*)

plusW16 :: Word16 -> Word16 -> Word16
plusW16 = (+)

minusW16 :: Word16 -> Word16 -> Word16
minusW16 = (-)

mulW16 :: Word16 -> Word16 -> Word16
mulW16 = (*)

plus32 :: Int32 -> Int32 -> Int32
plus32 = (+)

minus32 :: Int32 -> Int32 -> Int32
minus32 = (-)

mul32 :: Int32 -> Int32 -> Int32
mul32 = (*)

plusW32 :: Word32 -> Word32 -> Word32
plusW32 = (+)

minusW32 :: Word32 -> Word32 -> Word32
minusW32 = (-)

mulW32 :: Word32 -> Word32 -> Word32
mulW32 = (*)

plus64 :: Int64 -> Int64 -> Int64
plus64 = (+)

minus64 :: Int64 -> Int64 -> Int64
minus64 = (-)

mul64 :: Int64 -> Int64 -> Int64
mul64 = (*)

plusW64 :: Word64 -> Word64 -> Word64
plusW64 = (+)

minusW64 :: Word64 -> Word64 -> Word64
minusW64 = (-)

mulW64 :: Word64 -> Word64 -> Word64
mulW64 = (*)



