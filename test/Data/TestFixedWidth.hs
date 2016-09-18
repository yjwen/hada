{-# LANGUAGE TemplateHaskell #-}

module Data.TestFixedWidth (tests) where

import Distribution.TestSuite

import Data.FixedWidth
import Data.Bits
import Data.List
import Data.Ratio

$(declareFW "Bit7" 7 "bit7")
$(declareUFW "UBit7" 7 "ubit7")

testShow :: Result
testShow = allEqual [("Bit7 1", show $ bit7 129)]

testUShow :: Result
testUShow | result == ref = Pass
          | otherwise = Fail ("Result=" ++ result ++ ", ref=" ++ ref)
          where result = show $ ubit7 1
                ref = "UBit7 1"
allEqual :: (Eq a, Show a) => [(a, a)] -> Result
allEqual [] = Pass
allEqual ((r, t):xs) | r /= t = Fail $ "Ref=" ++ (show r) ++ ", test=" ++ (show t)
                     | otherwise = allEqual xs

testFromFW :: Result
testFromFW = allEqual $ map t [(0, 0), (1, 129)]
  where t (a, b) = (a, fromFW $ bit7 b)

testUFromFW :: Result
testUFromFW = allEqual $ map t [(0, 0), (1, 129)]
  where t (a, b) = (a, fromFW $ ubit7 b)

testFromEnum :: Result
testFromEnum = allEqual $ map t [ (7, bit 7 + 7), (-1, -1), (-64, -64), (63, 63)]
  where t (a, b) = (a, fromEnum $ bit7 b)

testUFromEnum :: Result
testUFromEnum = allEqual $ map t [(7, bit 7 + 7), (0, bit 7), (127, 127)]
  where t (a, b) = (a, fromEnum $ ubit7 b)

testBounded :: Result
testBounded = allEqual [ (-64, fromEnum (minBound::Bit7))
                       , (63, fromEnum (maxBound::Bit7))]

testUBounded :: Result
testUBounded = allEqual [ (0, fromFW (minBound::UBit7))
                        , (127, fromFW (maxBound::UBit7))
                        ]

testEq :: Result
testEq = allEqual $ map t [(0, 0), (63, 63), (64, (-64)), (-1, 127)]
  where t (a, b) = (bit7 a, bit7 b)

testUEq :: Result
testUEq = allEqual [ (ubit7 0, ubit7 0), (ubit7 0, ubit7 128)
                   , (ubit7 127, ubit7 127), (ubit7 127, ubit7 $ bit 7 + 127)]

testOrd :: Result
testOrd = allEqual ((zip (repeat LT) $ map t [(-1, 0), (0, 1), (64, -63)])
                    ++
                    (zip (repeat EQ) $ map t [(0, 0), (-1, -1), (63, 63), (64, -64)])
                    ++
                    (zip (repeat GT) $ map t [(0, -1), (1, 0), (-63, 64)]))
  where t (a, b) = compare (bit7 a) (bit7 b)


testUOrd :: Result
testUOrd = allEqual [ (LT, compare l r)
                    , (GT, compare r l)
                    , (EQ, compare l l)
                    ]
  where l = ubit7 0
        r = ubit7 127

testNum :: Result
testNum = allEqual [ (min, max + (bit7 1))
                   , (max, min + (bit7 $ -1))
                   , (min, max - (bit7 $ -1))
                   , (max, min - (bit7 1))
                   , (bit7 0, (bit7 1) * (bit7 128))
                   , (bit7 1, abs $ bit7 $ -1)
                   , (bit7 1, abs $ bit7 1)
                   , (bit7 $ -1, signum $ bit7 $ -1)
                   , (bit7 1, signum $ bit7 1)
                   , (bit7 1, negate $ bit7 $ -1)
                   , (bit7 $ -1, negate $ bit7 1)
                   ]
  where min = minBound::Bit7
        max = maxBound::Bit7

testUNum :: Result
testUNum = allEqual[ (min, max + one)
                   , (max, min - one)
                   , (max, max * one)
                   , (min, min * one)
                   , (max, - one)
                   , (min, abs min)
                   , (max, abs max)
                   ]
  where max = maxBound::UBit7
        min = minBound::UBit7
        one = ubit7 1

testReal :: Result
testReal = allEqual [(4 % 2, toRational $ bit7 2)]

testUReal :: Result
testUReal = allEqual [(4 % 2, toRational $ ubit7 2)]

testIntegral :: Result
testIntegral = allEqual $ zip ref test
  where vs = [(7, 2), (-7, 2), (-7, -2)]
        mref op = map (\ (a, b) -> bit7 $ op a b)
        mtest op = map (\ (a, b) -> op (bit7 a) (bit7 b))
        ref = (mref div vs) ++ (mref mod vs) ++ (mref quot vs) ++ (mref rem vs)
        test = (mtest div vs) ++ (mtest mod vs) ++ (mtest quot vs) ++ (mtest rem vs)

testUIntegral :: Result
testUIntegral = allEqual $ zip ref test
  where vs = [(7, 2)]
        mref op = map (\ (a, b) -> ubit7 $ op a b)
        mtest op = map (\ (a, b) -> op (ubit7 a) (ubit7 b))
        ref = (mref div vs) ++ (mref mod vs) ++ (mref quot vs) ++ (mref rem vs)
        test = (mtest div vs) ++ (mtest mod vs) ++ (mtest quot vs) ++ (mtest rem vs)


testBitOps :: Result
testBitOps = allEqual [ (bit7 0, bit7 1 .&. bit7 2)
                      , (bit7 3, bit7 1 .|. bit7 2)
                      , (bit7 1, bit7 2 `xor` bit7 3)
                      , (bit7 0, complement $ bit7 127)
                      , (bit7 2, shiftL (bit7 1) 1)
                      , (bit7 126, shiftL (bit7 127) 1)
                      , (bit7 1, shiftR (bit7 2) 1)
                      , (bit7 31, shiftR (bit7 63) 1)
                      , (bit7 129, rotateL (bit7 0x40) 1)
                      , (bit7 0x7e, rotateL (bit7 0xf7) 4)
                      , (bit7 0x40, rotateR (bit7 1) 1)
                      , (bit7 0x3f , rotateR (bit7 0xf7) 4)
                      ]

testBits :: Result
testBits = allEqual [ ("Just 7", show $ bitSizeMaybe $ bit7 1)
                    , ("True", show $ isSigned $ bit7 1)
                    , ("True", show $ testBit (bit7 32) 5)
                    , ("Bit7 32", show ((bit 5)::Bit7))
                    , ("7", show $ popCount $ bit7 (-1))
                    ]

testUBitOps :: Result
testUBitOps = allEqual [ (ubit7 0, ubit7 1 .&. ubit7 2)
                       , (ubit7 3, ubit7 1 .|. ubit7 2)
                       , (ubit7 1, ubit7 2 `xor` ubit7 3)
                       , (ubit7 0, complement $ ubit7 127)
                       , (ubit7 2, shiftL (ubit7 1) 1)
                       , (ubit7 126, shiftL (ubit7 127) 1)
                       , (ubit7 1, shiftR (ubit7 2) 1)
                       , (ubit7 31, shiftR (ubit7 63) 1)
                       , (ubit7 129, rotateL (ubit7 0x40) 1)
                       , (ubit7 0x7e, rotateL (ubit7 0xf7) 4)
                       , (ubit7 0x40, rotateR (ubit7 1) 1)
                       , (ubit7 0x3f , rotateR (ubit7 0xf7) 4)
                       ]

testUBits :: Result
testUBits = allEqual [ ("Just 7", show $ bitSizeMaybe $ ubit7 1)
                     , ("False", show $ isSigned $ ubit7 1)
                     , ("True", show $ testBit (ubit7 32) 5)
                     , ("UBit7 32", show ((bit 5)::UBit7))
                     , ("7", show $ popCount $ ubit7 (-1))
                     ]

signedTestInstance (test, name) = Test theInstance
  where theInstance = TestInstance { run = return $ Finished test
                                   , name = name
                                   , tags = ["Signed"]
                                   , options = []
                                   , setOption = \ _ _ -> Right theInstance
                                   }
unsignedTestInstance (test, name) = Test theInstance
  where theInstance = TestInstance { run = return $ Finished test
                                   , name = name
                                   , tags = ["Unsigned"]
                                   , options = []
                                   , setOption = \ _ _ -> Right theInstance
                                   }
tests :: IO [Test]
tests = return ( map signedTestInstance [ (testShow, "testShow")
                                        , (testFromFW, "testFromFW")
                                        , (testFromEnum, "testFromEnum")
                                        , (testBounded, "testBounded")
                                        , (testEq, "testEq")
                                        , (testOrd, "testOrd")
                                        , (testNum, "testNum")
                                        , (testReal, "testReal")
                                        , (testIntegral, "testIntegral")
                                        , (testBitOps, "testBitOps")
                                        , (testBits, "testBits")
                                        ]
                 ++
                 map unsignedTestInstance [ (testUShow, "testUShow")
                                          , (testUFromFW, "testUFromFW")
                                          , (testUFromEnum, "testUFromEnum")
                                          , (testUBounded, "testUBounded")
                                          , (testUEq, "testUEq")
                                          , (testUOrd, "testUOrd")
                                          , (testUNum, "testUNum")
                                          , (testUReal, "testUReal")
                                          , (testUIntegral, "testUIntegral")
                                          , (testUBitOps, "testUBitOps")
                                          , (testUBits, "testUBits")
                                          ])

