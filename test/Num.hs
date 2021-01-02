module Num(plus, plus8, plus16, plus32, plus64,
           plusU, plusU8, plusU16, plusU32, plusU64,
           minus, minus8, minus16, minus32, minus64,
           minusU, minusU8, minusU16, minusU32, minusU64,
           mul, mul8, mul16, mul32, mul64,
           mulU, mulU8, mulU16, mulU32, mulU64,
           neg, neg8, neg16, neg32, neg64,
           negU, negU8, negU16, negU32, negU64,
           abs_, abs8, abs16, abs32, abs64,
           absU, absU8, absU16, absU32, absU64,
           sig, sig8, sig16, sig32, sig64,
           sigU, sigU8, sigU16, sigU32, sigU64,
           eq, eq8, eq16, eq32, eq64,
           eqU, eqU8, eqU16, eqU32, eqU64,
           neq, neq8, neq16, neq32, neq64,
           neqU, neqU8, neqU16, neqU32, neqU64,
           lt, lt8, lt16, lt32, lt64,
           ltU, ltU8, ltU16, ltU32, ltU64,
           le, le8, le16, le32, le64,
           leU, leU8, leU16, leU32, leU64,
           gt, gt8, gt16, gt32, gt64,
           gtU, gtU8, gtU16, gtU32, gtU64,
           ge, ge8, ge16, ge32, ge64,
           geU, geU8, geU16, geU32, geU64,
           band, band8, band16, band32, band64,
           bandU, bandU8, bandU16, bandU32, bandU64,
           bor, bor8, bor16, bor32, bor64,
           borU, borU8, borU16, borU32, borU64,
           bxor, bxor8, bxor16, bxor32, bxor64,
           bxorU, bxorU8, bxorU16, bxorU32, bxorU64,
           bneg, bneg8, bneg16, bneg32, bneg64,
           bnegU, bnegU8, bnegU16, bnegU32, bnegU64,
           ) where

import Data.Int
import Data.Word
import Data.Bits

plus :: Int -> Int -> Int
plus = (+)

minus :: Int -> Int -> Int
minus = (-)

mul :: Int -> Int -> Int
mul = (*)

neg :: Int -> Int
neg = negate

abs_ :: Int -> Int
abs_ = abs

sig :: Int -> Int
sig = signum

plusU :: Word -> Word -> Word
plusU = (+)

minusU :: Word -> Word -> Word
minusU = (-)

mulU :: Word -> Word -> Word
mulU = (*)

negU :: Word -> Word
negU = negate

absU :: Word -> Word
absU = abs

sigU :: Word -> Word
sigU = signum

plus8 :: Int8 -> Int8 -> Int8
plus8 = (+)

minus8 :: Int8 -> Int8 -> Int8
minus8 = (-)

mul8 :: Int8 -> Int8 -> Int8
mul8 = (*)

neg8 :: Int8 -> Int8
neg8 = negate

abs8 :: Int8 -> Int8
abs8 = abs

sig8 :: Int8 -> Int8
sig8 = signum

plusU8 :: Word8 -> Word8 -> Word8
plusU8 = (+)

minusU8 :: Word8 -> Word8 -> Word8
minusU8 = (-)

mulU8 :: Word8 -> Word8 -> Word8
mulU8 = (*)

negU8 :: Word8 -> Word8
negU8 = negate

absU8 :: Word8 -> Word8
absU8 = abs

sigU8 :: Word8 -> Word8
sigU8 = signum

plus16 :: Int16 -> Int16 -> Int16
plus16 = (+)

minus16 :: Int16 -> Int16 -> Int16
minus16 = (-)

mul16 :: Int16 -> Int16 -> Int16
mul16 = (*)

neg16 :: Int16 -> Int16
neg16 = negate

abs16 :: Int16 -> Int16
abs16 = abs

sig16 :: Int16 -> Int16
sig16 = signum

plusU16 :: Word16 -> Word16 -> Word16
plusU16 = (+)

minusU16 :: Word16 -> Word16 -> Word16
minusU16 = (-)

mulU16 :: Word16 -> Word16 -> Word16
mulU16 = (*)

negU16 :: Word16 -> Word16
negU16 = negate

absU16 :: Word16 -> Word16
absU16 = abs

sigU16 :: Word16 -> Word16
sigU16 = signum

plus32 :: Int32 -> Int32 -> Int32
plus32 = (+)

minus32 :: Int32 -> Int32 -> Int32
minus32 = (-)

mul32 :: Int32 -> Int32 -> Int32
mul32 = (*)

neg32 :: Int32 -> Int32
neg32 = negate

abs32 :: Int32 -> Int32
abs32 = abs

sig32 :: Int32 -> Int32
sig32 = signum

plusU32 :: Word32 -> Word32 -> Word32
plusU32 = (+)

minusU32 :: Word32 -> Word32 -> Word32
minusU32 = (-)

mulU32 :: Word32 -> Word32 -> Word32
mulU32 = (*)

negU32 :: Word32 -> Word32
negU32 = negate

absU32 :: Word32 -> Word32
absU32 = abs

sigU32 :: Word32 -> Word32
sigU32 = signum

plus64 :: Int64 -> Int64 -> Int64
plus64 = (+)

minus64 :: Int64 -> Int64 -> Int64
minus64 = (-)

mul64 :: Int64 -> Int64 -> Int64
mul64 = (*)

neg64 :: Int64 -> Int64
neg64 = negate

abs64 :: Int64 -> Int64
abs64 = abs

sig64 :: Int64 -> Int64
sig64 = signum

plusU64 :: Word64 -> Word64 -> Word64
plusU64 = (+)

minusU64 :: Word64 -> Word64 -> Word64
minusU64 = (-)

mulU64 :: Word64 -> Word64 -> Word64
mulU64 = (*)

negU64 :: Word64 -> Word64
negU64 = negate

absU64 :: Word64 -> Word64
absU64 = abs

sigU64 :: Word64 -> Word64
sigU64 = signum

eq :: Int -> Int -> Bool
eq = (==)

eq8 :: Int8 -> Int8 -> Bool
eq8 = (==)

eq16 :: Int16 -> Int16 -> Bool
eq16 = (==)

eq32 :: Int32 -> Int32 -> Bool
eq32 = (==)

eq64 :: Int64 -> Int64 -> Bool
eq64 = (==)

eqU :: Word -> Word -> Bool
eqU = (==)

eqU8 :: Word8 -> Word8 -> Bool
eqU8 = (==)

eqU16 :: Word16 -> Word16 -> Bool
eqU16 = (==)

eqU32 :: Word32 -> Word32 -> Bool
eqU32 = (==)

eqU64 :: Word64 -> Word64 -> Bool
eqU64 = (==)

neq :: Int -> Int -> Bool
neq = (==)

neq8 :: Int8 -> Int8 -> Bool
neq8 = (==)

neq16 :: Int16 -> Int16 -> Bool
neq16 = (==)

neq32 :: Int32 -> Int32 -> Bool
neq32 = (==)

neq64 :: Int64 -> Int64 -> Bool
neq64 = (==)

neqU :: Word -> Word -> Bool
neqU = (==)

neqU8 :: Word8 -> Word8 -> Bool
neqU8 = (==)

neqU16 :: Word16 -> Word16 -> Bool
neqU16 = (==)

neqU32 :: Word32 -> Word32 -> Bool
neqU32 = (==)

neqU64 :: Word64 -> Word64 -> Bool
neqU64 = (==)

lt :: Int -> Int -> Bool
lt = (<)

lt8 :: Int8 -> Int8 -> Bool
lt8 = (<)

lt16 :: Int16 -> Int16 -> Bool
lt16 = (<)

lt32 :: Int32 -> Int32 -> Bool
lt32 = (<)

lt64 :: Int64 -> Int64 -> Bool
lt64 = (<)

ltU :: Word -> Word -> Bool
ltU = (<)

ltU8 :: Word8 -> Word8 -> Bool
ltU8 = (<)

ltU16 :: Word16 -> Word16 -> Bool
ltU16 = (<)

ltU32 :: Word32 -> Word32 -> Bool
ltU32 = (<)

ltU64 :: Word64 -> Word64 -> Bool
ltU64 = (<)

le :: Int -> Int -> Bool
le = (<=)

le8 :: Int8 -> Int8 -> Bool
le8 = (<=)

le16 :: Int16 -> Int16 -> Bool
le16 = (<=)

le32 :: Int32 -> Int32 -> Bool
le32 = (<=)

le64 :: Int64 -> Int64 -> Bool
le64 = (<=)

leU :: Word -> Word -> Bool
leU = (<=)

leU8 :: Word8 -> Word8 -> Bool
leU8 = (<=)

leU16 :: Word16 -> Word16 -> Bool
leU16 = (<=)

leU32 :: Word32 -> Word32 -> Bool
leU32 = (<=)

leU64 :: Word64 -> Word64 -> Bool
leU64 = (<=)

gt :: Int -> Int -> Bool
gt = (>)

gt8 :: Int8 -> Int8 -> Bool
gt8 = (>)

gt16 :: Int16 -> Int16 -> Bool
gt16 = (>)

gt32 :: Int32 -> Int32 -> Bool
gt32 = (>)

gt64 :: Int64 -> Int64 -> Bool
gt64 = (>)

gtU :: Word -> Word -> Bool
gtU = (>)

gtU8 :: Word8 -> Word8 -> Bool
gtU8 = (>)

gtU16 :: Word16 -> Word16 -> Bool
gtU16 = (>)

gtU32 :: Word32 -> Word32 -> Bool
gtU32 = (>)

gtU64 :: Word64 -> Word64 -> Bool
gtU64 = (>)

ge :: Int -> Int -> Bool
ge = (>=)

ge8 :: Int8 -> Int8 -> Bool
ge8 = (>=)

ge16 :: Int16 -> Int16 -> Bool
ge16 = (>=)

ge32 :: Int32 -> Int32 -> Bool
ge32 = (>=)

ge64 :: Int64 -> Int64 -> Bool
ge64 = (>=)

geU :: Word -> Word -> Bool
geU = (>=)

geU8 :: Word8 -> Word8 -> Bool
geU8 = (>=)

geU16 :: Word16 -> Word16 -> Bool
geU16 = (>=)

geU32 :: Word32 -> Word32 -> Bool
geU32 = (>=)

geU64 :: Word64 -> Word64 -> Bool
geU64 = (>=)

band :: Int -> Int -> Int
band = (.&.)

band8 :: Int8 -> Int8 -> Int8
band8 = (.&.)

band16 :: Int16 -> Int16 -> Int16
band16 = (.&.)

band32 :: Int32 -> Int32 -> Int32
band32 = (.&.)

band64 :: Int64 -> Int64 -> Int64
band64 = (.&.)

bandU :: Word -> Word -> Word
bandU = (.&.)

bandU8 :: Word8 -> Word8 -> Word8
bandU8 = (.&.)

bandU16 :: Word16 -> Word16 -> Word16
bandU16 = (.&.)

bandU32 :: Word32 -> Word32 -> Word32
bandU32 = (.&.)

bandU64 :: Word64 -> Word64 -> Word64
bandU64 = (.&.)

bor :: Int -> Int -> Int
bor = (.|.)

bor8 :: Int8 -> Int8 -> Int8
bor8 = (.|.)

bor16 :: Int16 -> Int16 -> Int16
bor16 = (.|.)

bor32 :: Int32 -> Int32 -> Int32
bor32 = (.|.)

bor64 :: Int64 -> Int64 -> Int64
bor64 = (.|.)

borU :: Word -> Word -> Word
borU = (.|.)

borU8 :: Word8 -> Word8 -> Word8
borU8 = (.|.)

borU16 :: Word16 -> Word16 -> Word16
borU16 = (.|.)

borU32 :: Word32 -> Word32 -> Word32
borU32 = (.|.)

borU64 :: Word64 -> Word64 -> Word64
borU64 = (.|.)

bxor :: Int -> Int -> Int
bxor = xor

bxor8 :: Int8 -> Int8 -> Int8
bxor8 = xor

bxor16 :: Int16 -> Int16 -> Int16
bxor16 = xor

bxor32 :: Int32 -> Int32 -> Int32
bxor32 = xor

bxor64 :: Int64 -> Int64 -> Int64
bxor64 = xor

bxorU :: Word -> Word -> Word
bxorU = xor

bxorU8 :: Word8 -> Word8 -> Word8
bxorU8 = xor

bxorU16 :: Word16 -> Word16 -> Word16
bxorU16 = xor

bxorU32 :: Word32 -> Word32 -> Word32
bxorU32 = xor

bxorU64 :: Word64 -> Word64 -> Word64
bxorU64 = xor

bneg :: Int -> Int
bneg = complement

bneg8 :: Int8 -> Int8
bneg8 = complement

bneg16 :: Int16 -> Int16
bneg16 = complement

bneg32 :: Int32 -> Int32
bneg32 = complement

bneg64 :: Int64 -> Int64
bneg64 = complement

bnegU :: Word -> Word
bnegU = complement

bnegU8 :: Word8 -> Word8
bnegU8 = complement

bnegU16 :: Word16 -> Word16
bnegU16 = complement

bnegU32 :: Word32 -> Word32
bnegU32 = complement

bnegU64 :: Word64 -> Word64
bnegU64 = complement
