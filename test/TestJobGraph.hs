import Test.HUnit
import System.Exit
import Algebra.Graph (Graph, edgeList)
import JobGraph

doJob :: Int -> ([Int], String)
doJob j = (if j > 0 then [] else [j + 1, j * 2], show j)

type IntJobGraph = JobGraph Int String

jobGraphTest = TestCase (assertEqual ""
                         [ (Left 0, Left 0)
                         , (Left 0, Left 1)
                         , (Left 0, Right "0")
                         , (Left 1, Right "1")]
                          (edgeList $ doAllJobs doJob $ job 0))

main = do counts <- runTestTT jobGraphTest
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
