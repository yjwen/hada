{-# LANGUAGE TemplateHaskell #-}
module TestFixedWidth (tests) where

import Distribution.TestSuite

import FixedWidth
import Data.Bits
import Data.List

$(declareFW "Bit7" "Bit7" 7)

testVector :: (Int -> Bit7 -> Result) -> [(Int, Bit7)] -> Result
testVector f [] = Pass
testVector f ((r, t):vs) | result == Pass = testVector f vs
                         | otherwise = result
  where result = f r t

testFromEnum :: Result
testFromEnum = testVector f vec
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
testEq = case (testVector f vec, testVector f' vec') of
           (Pass, Pass) -> Pass
           (Fail msg, _) -> Fail msg
           (_, Fail msg) -> Fail msg
  where vec = [(0, Bit7 0), (63, Bit7 63), (64, Bit7 (-64)), (-1, Bit7 127)]
        f r t | Bit7 r == t = Pass
              | otherwise = Fail ("When testing (==): r=" ++ (show r) ++ ", t=" ++ (show t))
        vec' = [(1, Bit7 0)]
        f' r t | Bit7 r /= t = Pass
               | otherwise = Fail ("When testing (/=): r=" ++ (show r) ++ ", t=" ++ (show t))

tests :: IO [Test]
tests = return [Test enumTest, Test boundedTest, Test eqTest]
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

