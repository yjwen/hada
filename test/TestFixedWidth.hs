{-# LANGUAGE TemplateHaskell #-}
module TestFixedWidth (tests) where

import Distribution.TestSuite

import FixedWidth
import Data.Bits
import Data.List
import Data.Ratio

$(declareFW "Bit7" "bit7" 7)
$(declareUFW "UBit7" "ubit7" 7)
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

testShow :: Result
testShow | result == ref = Pass
         | otherwise = Fail ("Result=" ++ result ++ ", ref=" ++ ref)
         where result = show $ bit7 1
               ref = "Bit7 1"

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
testFromFW = allEqual $ map t [(0, 0), (0, 128)]
  where t (a, b) = (a, fromFW $ bit7 b)

testFromEnum :: Result
testFromEnum = allEqual $ map t [ (7, bit 7 + 7), (-1, -1), (-64, -64), (63, 63)]
  where t (a, b) = (a, fromEnum $ bit7 b)

testUFromEnum :: Result
testUFromEnum = findFail $ map f vec
  where vec = [(7, U7 (bit 7 + 7)), (0, U7 (bit 7)), (127, U7 127)]
        f (r, t) | r == fromEnum t = Pass
                 | otherwise = Fail ("Result=" ++ (show $ fromEnum t) ++ ", ref=" ++ (show r))

testBounded :: Result
testBounded = allEqual [ (-64, fromEnum (minBound::Bit7))
                       , (63, fromEnum (maxBound::Bit7))]

testUBounded :: Result
testUBounded | min /= 0 = Fail ("min=" ++ (show min))
             | max /= 127 = Fail ("max=" ++ (show max))
             | otherwise = Pass
  where min = fromEnum (minBound::U7)
        max = fromEnum (maxBound::U7)

testEq :: Result
testEq = allEqual $ map t [(0, 0), (63, 63), (64, (-64)), (-1, 127)]
  where t (a, b) = (bit7 a, bit7 b)

testUEq :: Result
testUEq = findFail $ (map f vec) ++ (map f' vec')
  where vec = [(0, U7 0), (0, U7 128), (127, U7 127), (127, U7 $ bit 7 + 127)]
        f (r, t) | U7 r == t = Pass
                 | otherwise = Fail $ "When testing (==): r=" ++ (show r) ++ ", t=" ++ (show t)
        vec' = [(1, U7 0)]
        f' (r, t) | U7 r /= t = Pass
                  | otherwise = Fail $ "When testing (/=): r=" ++ (show r) ++ ", t=" ++ (show t)

testOrd :: Result
testOrd = allEqual ((zip (repeat LT) $ map t [(-1, 0), (0, 1), (64, -63)])
                    ++
                    (zip (repeat EQ) $ map t [(0, 0), (-1, -1), (63, 63), (64, -64)])
                    ++
                    (zip (repeat GT) $ map t [(0, -1), (1, 0), (-63, 64)]))
  where t (a, b) = compare (bit7 a) (bit7 b)


testUOrd :: Result
testUOrd | compare l r /= LT = fail l r "less than"
         | compare r l /= GT = fail l r "greater than"
         | compare l l /= EQ = fail l l "equal to"
         | otherwise = Pass
  where l = U7 0
        r = U7 127
        fail l r str = Fail $ (show l) ++ " is not " ++ str ++ " " ++ (show r)

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
testUNum | max + one /= min = fail (Just max) "+" one min
         -- | min - one /= max = fail (Just min) "-" one max
         | max * one /= max = fail (Just max) "*" one max
         | min * one /= min = fail (Just min) "*" one min
         -- - one /= max = fail Nothing "-" one max
         | abs min /= min = fail Nothing "abs" min min
         | abs max /= max = fail Nothing "abs" max max
         | otherwise = Pass
  where max = maxBound::U7
        min = minBound::U7
        one = U7 1
        fail :: Maybe U7 -> String -> U7 -> U7 -> Result
        fail l op r ref = Fail "Failed."
--         -- fail l op r ref = Fail $ l' ++ op ++ " " ++ (show r) ++ " /= " ++ (show ref)
--         --   where l'= case l of
--         --               Just x -> (show x) ++ " "
--         --               otherwise -> ""

testReal :: Result
testReal = allEqual [(4 % 2, toRational $ bit7 2)]

testIntegral :: Result
testIntegral = allEqual $ zip ref test
  where vs = [(7, 2), (-7, 2), (-7, -2)]
        mref op = map (\ (a, b) -> bit7 $ op a b)
        mtest op = map (\ (a, b) -> op (bit7 a) (bit7 b))
        ref = (mref div vs) ++ (mref mod vs) ++ (mref quot vs) ++ (mref rem vs)
        test = (mtest div vs) ++ (mtest mod vs) ++ (mtest quot vs) ++ (mtest rem vs)


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
                                        ]
                 ++
                 map unsignedTestInstance [ (testUShow, "testUShow")
                                          , (testUFromEnum, "testUFromEnum")
                                          , (testUBounded, "testUBounded")
                                          , (testUEq, "testUEq")
                                          , (testUOrd, "testUOrd")
                                          , (testUNum, "testUNum")
                                          ])

