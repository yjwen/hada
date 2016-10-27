module TestVerilog (tests) where
import Distribution.TestSuite

import Outputable
import Verilog

import Data.List.Split

tests :: IO [Test]
tests = do golden <- readFile "./test/module.v"
           return [Test $ testModuleInstance golden]

testModuleInstance :: String -> TestInstance
testModuleInstance golden =  TestInstance { run = return $ Finished $ testModule golden
                                          , name = "testModule"
                                          , tags = ["Verilog"]
                                          , options = []
                                          , setOption = \ _ _ -> Right $ testModuleInstance golden}

compareLines :: [String] -> [String] -> Result
compareLines a b = compareLines' (map trimLeft a) (map trimLeft b) 1
  where trimLeft = dropWhile (==' ')

compareLines' [] [] _ = Pass
compareLines' [] (t:tx) l = Fail $ "line " ++ (show l) ++ ": golden='', test='" ++ t ++ "'"
compareLines' (g:gx) [] l = Fail $ "line " ++ (show l) ++ ": golden='" ++ g ++ "', test=''"
compareLines' (g:gx) (t:tx) l | t == g = compareLines' gx tx (l + 1)
                              | otherwise = Fail $ "line = " ++ (show l) ++ ": golden='" ++ g ++ "', test='" ++ t ++ "'"

testModule :: String -> Result
testModule golden = compareLines (splitOn "\n" golden) (splitOn "\n" testContent)
  where testContent = showSDocUnsafe $ (ppr test) $$ (text "")

test = Module "abs" [a, b] [out] [Assign out' expCond]
  where a = Signal "a" $ Just (0, 63)
        b = Signal "b" $ Just (0, 63)
        out = Signal "out" $ Just (0, 63)
        a' = Signal "a" Nothing
        b' = Signal "b" Nothing
        out' = Signal "out" Nothing
        expCond = ConditionalExpr
                  (BinaryExpr LessThan expA expB)
                  (BinaryExpr Minus expB expA)
                  (BinaryExpr Minus expA expB)
        expA = SignalExpr a'
        expB = SignalExpr b'
