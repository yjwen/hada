import Test.HUnit
import System.Exit
import Algebra.Graph (Graph, edgeList)
import JobGraph
import TopoList(assertTopoListEq)

doJob :: Int -> ([Int], String)
doJob j = (if j > 5 then [] else [j + 1, j * 2 + 1], show j)

-- The example job graph used by tests
jg = doAllJobs doJob $ job 0

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
                          (edgeList jg))

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

main = do counts <- runTestTT $ TestList [ jobGraphTest
                                         , jobTopoFoldlTest
                                         , jobTopoFoldrTest
                                         , resultTopoFoldlTest
                                         , resultTopoFoldrTest
                                         ]
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
