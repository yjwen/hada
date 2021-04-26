import Test.HUnit
import System.Exit
import Algebra.Graph (Graph, vertexCount, empty, vertex, vertices, edge, edges, overlay)
import TopoGraph

oneV = vertex 0
oneE = edge 0 1
loop = edge 0 0
island = vertices [0, 1]
path2 = edges [(0, 1), (1, 2)]
loop2 = edges [(0, 1), (1, 0)]
converge2 = edges [(0, 1), (2, 1)]
diverge2 = edges [(0, 1), (0, 2)]
v_loop_v = overlay loop $ edges [(1, 0), (0, 2)]
v_loop2_v = overlay (edges [(2, 0), (1, 3)]) loop2



testIsTopoFree :: Graph Int -> [Bool] -> Test
testIsTopoFree g goldens
  = TestCase $ assertEqual "" goldens (map (\n -> isTopoFree n g) nodes)
  where nodes = take (1 + vertexCount g) (iterate (+1) 0)

isTopoFreeTests = TestList [
  testIsTopoFree empty [True],
  testIsTopoFree oneV [True, True],
  testIsTopoFree oneE [True, False, True],
  testIsTopoFree loop [False, True],
  testIsTopoFree island [True, True, True],
  testIsTopoFree path2 [True, False, False, True],
  testIsTopoFree loop2 [False, False, True],
  testIsTopoFree converge2 [True, False, True, True],
  testIsTopoFree diverge2 [True, False, False, True],
  testIsTopoFree v_loop_v [False, True, False, True],
  testIsTopoFree v_loop2_v [False, False, True, False, True]]

testFindTopoFirst :: Graph Int -> [Int] -> Test
testFindTopoFirst g candidates
  = TestCase (case result of
                Just a -> assertBool msg (a `elem` candidates)
                Nothing -> assertBool msg (candidates == []))
  where result = findTopoFirst g
        msg = "Found " ++ show result ++ ", but candidates are " ++ show candidates
findTopoFirstTests =
  TestList [ testFindTopoFirst empty []
           , testFindTopoFirst oneV [0]
           , testFindTopoFirst oneE [0]
           , testFindTopoFirst loop []
           , testFindTopoFirst island [0, 1]
           , testFindTopoFirst path2 [0]
           , testFindTopoFirst loop2 []
           , testFindTopoFirst converge2 [0, 2]
           , testFindTopoFirst diverge2 [0]
           , testFindTopoFirst v_loop_v [1]
           , testFindTopoFirst v_loop2_v [2]]

allTests = TestList [ TestLabel "isTopoFree" $ isTopoFreeTests
                    , TestLabel "findTopoFirst" $ findTopoFirstTests
                    ]


main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
