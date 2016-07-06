{-# LANGUAGE TemplateHaskell #-}
module TestFixedWidth (tests) where

import Distribution.TestSuite

import FixedWidth
import Data.Bits
import Data.List

$(declareFW "Bit7" "Bit7" 7)

testFromEnum :: Result
testFromEnum = case find findFail results of
                 Nothing -> Pass
                 Just x -> x
  where testPairs = [(7, bit 7 + 7), (-1, -1), (-64, -64), (63, 63)]
        testF (r, t) | r == result = Pass
                     | otherwise = Fail ("Result=" ++ (show result) ++ ", ref=" ++ (show r))
          where result = fromEnum $ Bit7 t
        results = map testF testPairs
        findFail Pass     = False
        findFail (Fail _) = True

testBounded :: Result
testBounded | min == -64 = Pass
            | otherwise = Fail ("min=" ++ (show min))
  where min = fromEnum (minBound::Bit7)
        max = fromEnum (maxBound::Bit7)

tests :: IO [Test]
tests = return [Test enumTest, Test boundedTest]
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

