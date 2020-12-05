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
            doc <- toSV Nothing $ mg_binds tidy
            putStrLn $ showSDocUnsafe doc
            return $ Finished Pass
