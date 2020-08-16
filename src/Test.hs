module Test(tests) where
import Distribution.TestSuite
import Syn
import HscTypes(mg_binds)
import Outputable
import SVerilog

tests :: IO [Test]
tests = return [Test testNum]

testNum :: TestInstance
testNum = TestInstance { run = runNum
                       , name = "Num"
                       , tags = []
                       , options = []
                       , setOption = \ _ _ -> Right testNum
                       }

runNum :: IO Progress
runNum = do tidy <- toTidy "test/Num.hs"
            docs <- mapM toSV $ mg_binds tidy
            let test_str = showSDocUnsafe $ vcat docs
            ref_str <- readFile "test/Num.sv"
            return $ Finished $ if test_str == ref_str
                                then Pass
                                else Fail $ "Incorrect:" ++ test_str ++ "<+>" ++ ref_str
