{-# LANGUAGE TemplateHaskell #-}
module TestFixedWidth (tests) where

import Distribution.TestSuite

import FixedWidth
import Data.Bits

$(declareFW "Bit7" "Bit7" 7)

testFromEnum :: Result
testFromEnum 
  | result == ref = Pass
  | otherwise = Fail ("Incorrect result of fromEnum. result=" ++ (show $ result) ++ ", ref=" ++ (show $ ref))
  where ref = 7
        testValue = Bit7 (bit 7 + ref)
        result = fromEnum testValue


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

