import Test.HUnit
import System.Exit
import Algebra.Graph ( Graph, vertexCount, empty, vertex, vertices, edge, edges, overlay
                     , edgeList)
import Data.List (nub)
import TopoGraph
import TopoList(assertTopoListEq)

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



testIsTopo :: (Int -> Graph Int -> Bool) -> Graph Int -> [Bool] -> Test
testIsTopo f g goldens
  = TestCase $ assertEqual "" goldens (map (\n -> f n g) nodes)
  where nodes = take (1 + vertexCount g) (iterate (+1) 0)

testIsTopoFirst = testIsTopo isTopoFirst

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

testIsTopoLast = testIsTopo isTopoLast
isTopoLastTests = TestList [
  testIsTopoLast empty [True],
  testIsTopoLast oneV [True, True],
  testIsTopoLast oneE [False, True, True],
  testIsTopoLast loop [False, True],
  testIsTopoLast island [True, True, True],
  testIsTopoLast path2 [False, False, True, True],
  testIsTopoLast loop2 [False, False, True],
  testIsTopoLast converge2 [False, True, False, True],
  testIsTopoLast diverge2 [False, True, True, True],
  testIsTopoLast v_loop_v [False, False, True, True],
  testIsTopoLast v_loop2_v [False, False, False, True, True]]

testFindTopo :: (Graph Int -> Maybe Int) -> Graph Int -> [Int] -> Test
testFindTopo f g candidates =
  TestCase (case result of
              Just a -> assertBool msg (a `elem` candidates)
              Nothing -> assertBool msg (candidates == []))
  where result = f g
        msg = "Found " ++ show result ++ ", but candidates are " ++ show candidates

testFindTopoFirst = testFindTopo findTopoFirst
testFindTopoLast = testFindTopo findTopoLast
testFindTopoFirstTrue = testFindTopo (findTopoFirstTrue even)
testFindTopoLastTrue = testFindTopo (findTopoLastTrue even)

findTopoFirstTests =
  TestList [ testFindTopoFirst empty []
           , testFindTopoFirst oneV [0]
           , testFindTopoFirst oneE [0]
           , testFindTopoFirst loop [0]
           , testFindTopoFirst island [0, 1]
           , testFindTopoFirst path2 [0]
           , testFindTopoFirst loop2 []
           , testFindTopoFirst converge2 [0, 2]
           , testFindTopoFirst diverge2 [0]
           , testFindTopoFirst v_loop_v [1]
           , testFindTopoFirst v_loop2_v [2]]

findTopoFirstTrueTests =
  TestList [ testFindTopoFirstTrue empty []
           , testFindTopoFirstTrue oneV [0]
           , testFindTopoFirstTrue oneE [0]
           , testFindTopoFirstTrue loop [0]
           , testFindTopoFirstTrue island [0]
           , testFindTopoFirstTrue path2 [0]
           , testFindTopoFirstTrue loop2 []
           , testFindTopoFirstTrue converge2 [0]
           , testFindTopoFirstTrue diverge2 [0]
           , testFindTopoFirstTrue v_loop_v []
           , testFindTopoFirstTrue v_loop2_v [2]]

findTopoLastTests =
  TestList [ testFindTopoLast empty []
           , testFindTopoLast oneV [0]
           , testFindTopoLast oneE [1]
           , testFindTopoLast loop [0]
           , testFindTopoLast island [0, 1]
           , testFindTopoLast path2 [2]
           , testFindTopoLast loop2 []
           , testFindTopoLast converge2 [1]
           , testFindTopoLast diverge2 [1, 2]
           , testFindTopoLast v_loop_v [2]
           , testFindTopoLast v_loop2_v [3]]

findTopoLastTrueTests =
  TestList [ testFindTopoLastTrue empty []
           , testFindTopoLastTrue oneV [0]
           , testFindTopoLastTrue oneE []
           , testFindTopoLastTrue loop [0]
           , testFindTopoLastTrue island [0]
           , testFindTopoLastTrue path2 [2]
           , testFindTopoLastTrue loop2 []
           , testFindTopoLastTrue converge2 []
           , testFindTopoLastTrue diverge2 [2]
           , testFindTopoLastTrue v_loop_v [2]
           , testFindTopoLastTrue v_loop2_v []]

testTopoFoldl :: Graph Int -> [(Int, Int)] -> [(Int, Int)] -> Test
testTopoFoldl g topoEdges cyclicEdges =
  let (topoList, cyclicGraph) = topoFoldl (flip (:)) [] g
  in TestCase (do assertEqual "Duplicated topoList" (nub topoList) topoList
                  assertTopoListEq topoList topoEdges
                  assertEqual "" cyclicEdges (edgeList cyclicGraph))

topoFoldlTests = TestList [ testTopoFoldl empty [] []
                          , testTopoFoldl oneV [(0, 0)] []
                          , testTopoFoldl oneE [(1, 0)] []
                          , testTopoFoldl loop [(0, 0)] []
                          , testTopoFoldl island [(0, 0), (1, 1)] []
                          , testTopoFoldl path2 [(2, 1), (1, 0)] []
                          , testTopoFoldl loop2 [] [(0, 1), (1, 0)]
                          , testTopoFoldl converge2 [(1, 0), (1, 2)] []
                          , testTopoFoldl diverge2 [(1, 0), (2, 0)] []
                          , testTopoFoldl v_loop_v [(2, 0), (0, 1)] []
                          , testTopoFoldl v_loop2_v [(2, 2)] [(0, 1), (1, 0), (1, 3)]
                          ]
  

testTopoFoldr :: Graph Int -> [(Int, Int)] -> [(Int, Int)] -> Test
testTopoFoldr g topoEdges cyclicEdges =
  let (topoList, cyclicGraph) = topoFoldr (:) [] g
  in TestCase (do assertEqual "Duplicated topoList" (nub topoList) topoList
                  assertTopoListEq topoList topoEdges
                  assertEqual ""  cyclicEdges (edgeList cyclicGraph))

topoFoldrTests = TestList [ testTopoFoldr empty [] []
                          , testTopoFoldr oneV [(0, 0)] []
                          , testTopoFoldr oneE [(0, 1)] []
                          , testTopoFoldr loop [(0, 0)] []
                          , testTopoFoldr island [(0, 0), (1, 1)] []
                          , testTopoFoldr path2 [(0, 1), (1, 2)] []
                          , testTopoFoldr loop2 [] [(0, 1), (1, 0)]
                          , testTopoFoldr converge2 [(0, 1), (2, 1)] []
                          , testTopoFoldr diverge2 [(0, 1), (0, 2)] []
                          , testTopoFoldr v_loop_v [(1, 0), (0, 2)] []
                          , testTopoFoldr v_loop2_v [(3, 3)] [(0, 1), (1, 0), (2, 0)]]

allTests = TestList [ TestLabel "isTopoFirst" $ isTopoFirstTests
                    , TestLabel "isTopoLast" $ isTopoLastTests
                    , TestLabel "findTopoFirst" $ findTopoFirstTests
                    , TestLabel "findTopoFirstTrue" $ findTopoFirstTrueTests
                    , TestLabel "findTopoLast" $ findTopoLastTests
                    , TestLabel "findTopoLastTrue" $ findTopoLastTrueTests
                    , TestLabel "topoFoldlTests" $ topoFoldlTests
                    , TestLabel "topoFoldrTests" $ topoFoldrTests
                    ]


main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
