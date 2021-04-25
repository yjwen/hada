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



testIsTopoFirst :: Graph Int -> [Bool] -> Test
testIsTopoFirst g goldens
  = TestCase $ assertEqual "" goldens (map (\n -> isTopoFirst n g) nodes)
  where nodes = take (1 + vertexCount g) (iterate (+1) 0)

isTopoFirstTests = TestList [
  testIsTopoFirst empty [True],
  testIsTopoFirst oneV [True, True],
  testIsTopoFirst oneE [True, False, True],
  testIsTopoFirst loop [False, True],
  testIsTopoFirst island [True, True, True],
  testIsTopoFirst path2 [True, False, False, True],
  testIsTopoFirst loop2 [False, False, True],
  testIsTopoFirst converge2 [True, False, True, True],
  testIsTopoFirst diverge2 [True, False, False, True],
  testIsTopoFirst v_loop_v [False, True, False, True],
  testIsTopoFirst v_loop2_v [False, False, True, False, True]]


allTests = TestList [TestLabel "isTopoFirst" $ isTopoFirstTests]


main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
