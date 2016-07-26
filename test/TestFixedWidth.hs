{-# LANGUAGE TemplateHaskell #-}
module TestFixedWidth (tests) where

import Distribution.TestSuite

import FixedWidth
import Data.Bits
import Data.List
import Data.Ratio

$(declareFW "Bit7" "Bit7" 7)

$(declareUnsignedFW "U7" "U7" 7)

findFail :: [Result] -> Result
findFail rs = case find f rs of
                Nothing -> Pass
                Just x -> x
  where f Pass = False
        f (Fail _) = True
  

testVector :: [(a, b)] -> (a -> b -> Result) -> Result
testVector [] f = Pass
testVector ((r, t):vs) f | result == Pass = testVector vs f
                         | otherwise = result
  where result = f r t

testFromEnum :: Result
testFromEnum = findFail $ map f vec
  where vec = [(7, Bit7 (bit 7 + 7)), (-1, Bit7 $ -1), (-64, Bit7 $ -64), (63, Bit7 63)]
        f (r, t) | r == fromEnum t =  Pass
                 | otherwise =  Fail ("Result=" ++ (show $ fromEnum t) ++ ", ref=" ++ (show r))

testUFromEnum :: Result
testUFromEnum = findFail $ map f vec
  where vec = [(7, U7 (bit 7 + 7)), (0, U7 (bit 7)), (127, U7 127)]
        f (r, t) | r == fromEnum t = Pass
                 | otherwise = Fail ("Result=" ++ (show $ fromEnum t) ++ ", ref=" ++ (show r))

testBounded :: Result
testBounded | min == -64 = Pass
            | otherwise = Fail ("min=" ++ (show min))
  where min = fromEnum (minBound::Bit7)
        max = fromEnum (maxBound::Bit7)

testEq :: Result
testEq = findFail $ (map f vec) ++ (map f' vec')
  where vec = [(0, Bit7 0), (63, Bit7 63), (64, Bit7 (-64)), (-1, Bit7 127)]
        f (r, t) | Bit7 r == t = Pass
                 | otherwise = Fail ("When testing (==): r=" ++ (show r) ++ ", t=" ++ (show t))
        vec' = [(1, Bit7 0)]
        f' (r, t) | Bit7 r /= t = Pass
                  | otherwise = Fail ("When testing (/=): r=" ++ (show r) ++ ", t=" ++ (show t))

testOrd :: Result
testOrd = findFail $ map f vec
  where vec = [(Bit7 (-1), Bit7 0), (Bit7 0, Bit7 1), (Bit7 64, Bit7 (-63))]
        f (l, r) | compare l r == LT = Pass
                 | otherwise = Fail ((show l) ++ " is not less than " ++ (show r))

testNum :: Result
testNum = findFail $ (map (f (+) "+") vecPlus) ++ (map (f (-) "-") vecMinus) ++ (map (f (*) "*") vecMul) ++
          (map (f' abs "abs") vecAbs) ++ (map (f' negate "negate") vecNegate)
  where vecPlus = [ (minBound::Bit7, maxBound::Bit7, Bit7 1)
                  , (maxBound::Bit7, minBound::Bit7, Bit7 $ -1)
                  ]
        vecMinus = [ (minBound::Bit7, maxBound::Bit7, Bit7 $ -1)
                   , (maxBound::Bit7, minBound::Bit7, Bit7 1)
                   ]
        vecMul = [ (Bit7 0, Bit7 1, Bit7 128)]
        f op opstr (r, lhs, rhs) | r == op lhs rhs = Pass
                                 | otherwise = Fail $ "Found " ++ (show lhs) ++ opstr ++ (show rhs) ++
                                               " /= " ++ (show r)
        vecAbs = [ (Bit7 1, Bit7 $ -1)
                 , (Bit7 1, Bit7 1)
                 ]
        vecNegate = [ (Bit7 1, Bit7 $ -1)
                    , (Bit7 $ -1, Bit7 1)
                    ]
        f' op opstr (r, t) | r == op t = Pass
                           | otherwise = Fail $ "Found " ++ opstr ++ (show t) ++ " /= " ++ (show r)
testReal :: Result
testReal | ref == test = Pass
         | otherwise = Fail $ "ref=" ++ (show ref) ++ ", test=" ++ (show test)
  where ref = 4 % 2
        test = toRational $ Bit7 2


testIntegral :: Result
testIntegral = findFail $ map f vs
  where vs = [(7, 2), (-7, 2), (-7, -2)]
        f (a, b) | dab /= dab' = mesg "div" dab' dab
                 | mab /= mab' = mesg "mod" mab' mab
                 | qab /= qab' = mesg "quot" qab' qab
                 | rab /= rab' = mesg "rem" rab' rab
                 | otherwise = Pass
          where a' = Bit7 a
                b' = Bit7 b
                dab = Bit7 $ a `div` b
                dab' = a' `div` b'
                mab = Bit7 $ a `mod` b
                mab' = a' `mod` b'
                qab = Bit7 $ a `quot` b
                qab' = a' `quot` b'
                rab = Bit7 $ a `rem` b
                rab' = a' `rem` b'
                mesg  op ab' ab = Fail $ op ++ " " ++ (show a') ++ " " ++ (show b') ++ " should be " ++ (show $ ab) ++ ", but got " ++ (show ab')

signedTestInstance test name = Test theInstance
  where theInstance = TestInstance { run = return $ Finished test
                                   , name = name
                                   , tags = ["Signed"]
                                   , options = []
                                   , setOption = \ _ _ -> Right theInstance
                                   }
unsignedTestInstance test name = Test theInstance
  where theInstance = TestInstance { run = return $ Finished test
                                   , name = name
                                   , tags = ["Unsigned"]
                                   , options = []
                                   , setOption = \ _ _ -> Right theInstance
                                   }
tests :: IO [Test]
tests = return $ map Test [enumTest, boundedTest, eqTest, ordTest, numTest, realTest, integralTest, enumUTest]
  where
    enumTest = TestInstance
      { run = return $ Finished testFromEnum
      , name = "testFromEnum"
      , tags = ["Signed"]
      , options = []
      , setOption = \ _ _ -> Right enumTest
      }
    boundedTest = TestInstance
      { run = return $ Finished testBounded
      , name = "testBounded"
      , tags = ["Signed"]
      , options = []
      , setOption = \ _ _ -> Right boundedTest
      }
    eqTest = TestInstance
      { run = return $ Finished testEq
      , name = "testEq"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right eqTest
      }
    ordTest = TestInstance
      { run = return $ Finished testOrd
      , name = "testOrd"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right ordTest
      }
    numTest = TestInstance
      { run = return $ Finished testNum
      , name = "testNum"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right numTest
      }
    realTest = TestInstance
      { run = return $ Finished testReal
      , name = "testReal"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right realTest
      }
    integralTest = TestInstance
      { run = return $ Finished testIntegral
      , name = "testIntegral"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right integralTest
      }
    enumUTest = TestInstance
      { run = return $ Finished testUFromEnum
      , name = "testUFromEnum"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right enumUTest
      }
