import Test.HUnit
import System.Exit
import Algebra.Graph (Graph, edgeList)
import JobGraph

doJob :: Int -> ([Int], String)
doJob j = (if j > 5 then [] else [j + 1, j * 2 + 1], show j)

type IntJobGraph = JobGraph Int String

jobGraphTest = TestCase (assertEqual ""
                         [ (Left 0, Right "0")
                         , (Left 1, Right "1")
                         , (Left 2, Right "2")
                         , (Left 3, Right "3")
                         , (Left 4, Right "4")
                         , (Left 5, Right "5")
                         , (Left 6, Right "6")
                         , (Left 7, Right "7")
                         , (Left 9, Right "9")
                         , (Left 11, Right "11")
                         , (Right "0", Left 1)
                         , (Right "1", Left 2)
                         , (Right "1", Left 3)
                         , (Right "2", Left 3)
                         , (Right "2", Left 5)
                         , (Right "3", Left 4)
                         , (Right "3", Left 7)
                         , (Right "4", Left 5)
                         , (Right "4", Left 9)
                         , (Right "5", Left 6)
                         , (Right "5", Left 11)
                         ]
                          (edgeList $ doAllJobs doJob $ job 0))

main = do counts <- runTestTT jobGraphTest
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
