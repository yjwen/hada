import Test.HUnit
import System.Exit
import Algebra.Graph (Graph, edgeList, vertexList)
import JobGraph
import TopoList(assertTopoListEq)

doJob :: Int -> Int -> ([Int], String)
doJob t j = (if j >= t then [] else [j + 1, j * 2 + 1], show j)


initJG = vertexTodo 0

testDoAllJobs0 = TestCase (assertEqual
                           ""
                           [ExactJR (done 0 "0")]
                           (map ExactJR $ vertexList $ doAllJobs (doJob 0) initJG))
testDoAllJobs1 = TestCase (assertEqual
                           ""
                           [exactEdge (done 0 "0", done 1 "1")]
                           (map exactEdge $ edgeList $ doAllJobs (doJob 1) initJG))

testDoAllJobs2 = TestCase (assertEqual
                           ""
                           (map exactEdge [(done 0 "0", done 1 "1"),
                                           (done 1 "1", done 2 "2"),
                                           (done 1 "1", done 3 "3")])
                           (map exactEdge $ edgeList $ doAllJobs (doJob 2) initJG))

testDoAllJobs3 = TestCase (assertEqual
                           ""
                           (map exactEdge [(done 0 "0", done 1 "1"),
                                           (done 1 "1", done 2 "2"),
                                           (done 1 "1", done 3 "3"),
                                           (done 2 "2", done 3 "3"),
                                           (done 2 "2", done 5 "5")])
                           (map exactEdge $ edgeList $ doAllJobs (doJob 3) initJG))

testDoAllJobs4 = TestCase (assertEqual
                           ""
                           (map exactEdge [(done 0 "0", done 1 "1"),
                                           (done 1 "1", done 2 "2"),
                                           (done 1 "1", done 3 "3"),
                                           (done 2 "2", done 3 "3"),
                                           (done 2 "2", done 5 "5"),
                                           (done 3 "3", done 4 "4"),
                                           (done 3 "3", done 7 "7")])
                           (map exactEdge $ edgeList $ doAllJobs (doJob 4) initJG))

testDoAllJobs5 = TestCase (assertEqual
                           ""
                           (map exactEdge [(done 0 "0", done 1 "1"),
                                           (done 1 "1", done 2 "2"),
                                           (done 1 "1", done 3 "3"),
                                           (done 2 "2", done 3 "3"),
                                           (done 2 "2", done 5 "5"),
                                           (done 3 "3", done 4 "4"),
                                           (done 3 "3", done 7 "7"),
                                           (done 4 "4", done 5 "5"),
                                           (done 4 "4", done 9 "9")])
                           (map exactEdge $ edgeList $ doAllJobs (doJob 5) initJG))

testDoAllJobs6 = TestCase (assertEqual
                           ""
                           (map exactEdge [(done 0 "0", done 1 "1"),
                                           (done 1 "1", done 2 "2"),
                                           (done 1 "1", done 3 "3"),
                                           (done 2 "2", done 3 "3"),
                                           (done 2 "2", done 5 "5"),
                                           (done 3 "3", done 4 "4"),
                                           (done 3 "3", done 7 "7"),
                                           (done 4 "4", done 5 "5"),
                                           (done 4 "4", done 9 "9"),
                                           (done 5 "5", done 6 "6"),
                                           (done 5 "5", done 11 "11")])
                           (map exactEdge $ edgeList $ doAllJobs (doJob 6) initJG))
-- The example job graph used by tests
jg = doAllJobs (doJob 6) $ vertexTodo 0

type R = JobRecord Int String
type ExactR = ExactJR Int String
exactEdge :: (R, R) -> (ExactR, ExactR)
exactEdge (r0, r1) = (ExactJR r0, ExactJR r1)

jobTopoFoldlTest = let (topoList, cyclicGraph) = jobTopoFoldl (flip (:)) [] jg
                   in TestCase (do assertTopoListEq topoList [ (1, 0)
                                                             , (2, 1)
                                                             , (3, 2)
                                                             , (4, 3)
                                                             , (5, 4)
                                                             , (9, 4)
                                                             , (6, 5)
                                                             , (11, 5)
                                                             ]
                                   assertEqual "" [] (edgeList cyclicGraph))
jobTopoFoldrTest = let (topoList, cyclicGraph) = jobTopoFoldr (:) [] jg
                   in TestCase (do assertTopoListEq topoList [ (0, 1)
                                                             , (1, 2)
                                                             , (2, 3)
                                                             , (3, 4)
                                                             , (4, 5)
                                                             , (4, 9)
                                                             , (5, 6)
                                                             , (5, 11)
                                                             ]
                                   assertEqual "" [] (edgeList cyclicGraph))

resultTopoFoldlTest = let (topoList, cyclicGraph) = resultTopoFoldl (flip (:)) [] jg
                      in TestCase (do assertTopoListEq topoList [ ("1", "0")
                                                                , ("2", "1")
                                                                , ("3", "2")
                                                                , ("4", "3")
                                                                , ("5", "4")
                                                                , ("9", "4")
                                                                , ("6", "5")
                                                                , ("11", "5")
                                                                ]
                                      assertEqual "" [] (edgeList cyclicGraph))

resultTopoFoldrTest = let (topoList, cyclicGraph) = resultTopoFoldr (:) [] jg
                      in TestCase (do assertTopoListEq topoList [ ("0", "1")
                                                                , ("1", "2")
                                                                , ("2", "3")
                                                                , ("3", "4")
                                                                , ("4", "5")
                                                                , ("4", "9")
                                                                , ("5", "6")
                                                                , ("5", "11")
                                                                ]
                                      assertEqual "" [] (edgeList cyclicGraph))

testMustSetDone = TestCase (assertEqual
                            ""
                            [ExactJR (todo 0),
                             ExactJR (done 1 "1")]
                            (map ExactJR $ vertexList (mustSetDone 1 "1" (vertexTodo 0))))

main = do counts <- runTestTT $ TestList [ testDoAllJobs0
                                         , testDoAllJobs1
                                         , testDoAllJobs2
                                         , testDoAllJobs3
                                         , testDoAllJobs4
                                         , testDoAllJobs5
                                         , testDoAllJobs6
                                         , jobTopoFoldlTest
                                         , jobTopoFoldrTest
                                         , resultTopoFoldlTest
                                         , resultTopoFoldrTest
                                         , testMustSetDone
                                         ]
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
