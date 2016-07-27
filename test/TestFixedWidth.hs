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
testBounded | min /= -64 = Fail ("min=" ++ (show min))
            | max /= 63 = Fail ("max=" ++ (show max))
            | otherwise = Pass
  where min = fromEnum (minBound::Bit7)
        max = fromEnum (maxBound::Bit7)

testUBounded :: Result
testUBounded | min /= 0 = Fail ("min=" ++ (show min))
             | max /= 127 = Fail ("max=" ++ (show max))
             | otherwise = Pass
  where min = fromEnum (minBound::U7)
        max = fromEnum (maxBound::U7)

testEq :: Result
testEq = findFail $ (map f vec) ++ (map f' vec')
  where vec = [(0, Bit7 0), (63, Bit7 63), (64, Bit7 (-64)), (-1, Bit7 127)]
        f (r, t) | Bit7 r == t = Pass
                 | otherwise = Fail ("When testing (==): r=" ++ (show r) ++ ", t=" ++ (show t))
        vec' = [(1, Bit7 0)]
        f' (r, t) | Bit7 r /= t = Pass
                  | otherwise = Fail ("When testing (/=): r=" ++ (show r) ++ ", t=" ++ (show t))

testUEq :: Result
testUEq = findFail $ (map f vec) ++ (map f' vec')
  where vec = [(0, U7 0), (0, U7 128), (127, U7 127), (127, U7 $ bit 7 + 127)]
        f (r, t) | U7 r == t = Pass
                 | otherwise = Fail $ "When testing (==): r=" ++ (show r) ++ ", t=" ++ (show t)
        vec' = [(1, U7 0)]
        f' (r, t) | U7 r /= t = Pass
                  | otherwise = Fail $ "When testing (/=): r=" ++ (show r) ++ ", t=" ++ (show t)

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
tests = return ( map signedTestInstance [ (testFromEnum, "testFromEnum")
                                        , (testBounded, "testBounded")
                                        , (testEq, "testEq")
                                        , (testOrd, "testOrd")
                                        , (testNum, "testNum")
                                        , (testReal, "testReal")
                                        , (testIntegral, "testIntegral")
                                        ]
                 ++
                 map unsignedTestInstance [ (testUFromEnum, "testUFromEnum")
                                          , (testUBounded, "testUBounded")
                                          , (testUEq, "testUEq")
                                          ])

