module TopoList (hasEdge, assertTopoListEq) where

import Test.HUnit

-- | An edge x→y is in a topo list l when both x and y are in l, and x
-- appears before y.
hasEdge :: (Eq a, Show a) => [a] -> (a, a) -> Assertion
hasEdge l (x, y) = assertBool msg (y `elem` dropWhile (/= x) l)
  where msg = ("Edge " ++ show (x, y) ++ " is not in topo list " ++ show l)

assertTopoListEq :: (Eq a, Show a) => [a] -> [(a, a)] -> Assertion
assertTopoListEq l edges = mapM (l `hasEdge`) edges >> return ()

