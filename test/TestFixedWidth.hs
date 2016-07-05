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
  where testPairs = [(7, bit 7 + 7), (-1, -1)]
        testF (r, t) | r == result = Pass
                     | otherwise = Fail ("Incorrect result of fromEnum, result=" ++ (show result) ++
                                         ", ref=" ++ (show r))
          where result = fromEnum $ Bit7 t
        results = map testF testPairs
        findFail Pass     = False
        findFail (Fail _) = True

tests :: IO [Test]
tests = return [Test $ enumTest]
  where
    enumTest = TestInstance
      {run = return $ Finished testFromEnum
      , name = "testFromEnum"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right enumTest
      }

