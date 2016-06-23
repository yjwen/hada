{-# LANGUAGE TemplateHaskell #-}
module TestFixedWidth (tests) where

import Distribution.TestSuite

import FixedWidth
import Data.Bits

$(declareFW "Bit7" "Bit7" 7)
-- data Bit7 = Bit7 Int

toInt (Bit7 a) = a .&. 127

testToInt :: Result
testToInt 
  | result == ref = Pass
  | otherwise = Fail ("Incorrect conversion to Int. result=" ++ (show $ result) ++ ", ref=" ++ (show $ ref))
  where result = toInt $ Bit7 (128 + ref)
        ref = 7

tests :: IO [Test]
tests = return [Test $ toIntTest]
  where
    toIntTest = TestInstance
      {run = return $ Finished testToInt
      , name = "testToInt"
      , tags = []
      , options = []
      , setOption = \ _ _ -> Right toIntTest
      }

