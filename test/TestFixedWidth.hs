{-# LANGUAGE TemplateHaskell #-}
module TestFixedWidth (tests) where

import Distribution.TestSuite

import FixedWidth
import Data.Bits
import Data.List

$(declareFW "Bit7" "Bit7" 7)

testVector :: [(a, b)] -> (a -> b -> Result) -> Result
testVector [] f = Pass
testVector ((r, t):vs) f | result == Pass = testVector vs f
                         | otherwise = result
  where result = f r t

testFromEnum :: Result
testFromEnum = testVector vec f
  where vec = [(7, Bit7 (bit 7 + 7)), (-1, Bit7 $ -1), (-64, Bit7 $ -64), (63, Bit7 63)]
        f r t | r == result = Pass
              | otherwise = Fail ("Result=" ++ (show result) ++ ", ref=" ++ (show r))
          where result = fromEnum t

testBounded :: Result
testBounded | min == -64 = Pass
            | otherwise = Fail ("min=" ++ (show min))
  where min = fromEnum (minBound::Bit7)
        max = fromEnum (maxBound::Bit7)

testEq :: Result
testEq = case (testVector vec f, testVector vec' f') of
           (Pass, Pass) -> Pass
           (Fail msg, _) -> Fail msg
           (_, Fail msg) -> Fail msg
  where vec = [(0, Bit7 0), (63, Bit7 63), (64, Bit7 (-64)), (-1, Bit7 127)]
        f r t | Bit7 r == t = Pass
              | otherwise = Fail ("When testing (==): r=" ++ (show r) ++ ", t=" ++ (show t))
        vec' = [(1, Bit7 0)]
        f' r t | Bit7 r /= t = Pass
               | otherwise = Fail ("When testing (/=): r=" ++ (show r) ++ ", t=" ++ (show t))

testOrd :: Result
testOrd = testVector vec f
  where vec = [(Bit7 (-1), Bit7 0), (Bit7 0, Bit7 1), (Bit7 64, Bit7 (-63))]
        f l r | compare l r == LT = Pass
              | otherwise = Fail ((show l) ++ " is not less than " ++ (show r))

tests :: IO [Test]
tests = return [Test enumTest, Test boundedTest, Test eqTest, Test ordTest]
  where
    enumTest = TestInstance
      {run = return $ Finished testFromEnum
      , name = "testFromEnum"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right enumTest
      }
    boundedTest = TestInstance
      {run = return $ Finished testBounded
      , name = "testBounded"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right boundedTest
      }
    eqTest = TestInstance
      {run = return $ Finished testEq
      , name = "testEq"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right eqTest
      }
    ordTest = TestInstance
      {run = return $ Finished testOrd
      , name = "testOrd"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right ordTest
      }

