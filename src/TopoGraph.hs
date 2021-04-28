module TopoGraph (isTopoFree, findTopoFirst, topoFoldl) where

import Algebra.Graph (Graph(..), hasVertex, removeVertex)

-- | isTopoFree x g == true if there is no edge y→x (y /= x) exists in
-- graph g. Note any node that is not a node in g is a topo-free node
-- to g. And a node x∈g that has only incoming edge x→x is also
-- topo-free to g
isTopoFree :: (Eq a) => a -> Graph a -> Bool
isTopoFree a Empty = True
isTopoFree a (Vertex b) = True
isTopoFree a (Overlay left  right) = isTopoFree a left && isTopoFree a right
isTopoFree a (Connect left Empty) = isTopoFree a left
isTopoFree a (Connect left right) = isTopoFree a left && (not $ hasVertex a right)

-- | findTopoFirst g tries to find a node x∈g that is topo-free to g
-- If no such node is found, return Nothing, otherwise return Just x.
findTopoFirst :: (Eq a) => Graph a -> Maybe a
findTopoFirst g = findTopoFirst' g Empty

-- | findTopoFirst' g g' tries to find a node x∈g that is topo-free to
-- both g and g'. If no such node is found, return Nothing, otherwise
-- return Just x
findTopoFirst' :: (Eq a) => Graph a -> Graph a -> Maybe a
findTopoFirst' Empty _ = Nothing
findTopoFirst' (Vertex b) g' = if isTopoFree b g' then Just b else Nothing
findTopoFirst' (Overlay left right) g' =
  case findTopoFirst' left (Overlay right g') of
    Just a -> Just a
    Nothing -> findTopoFirst' right (Overlay left g')
findTopoFirst' (Connect left right) g' =  findTopoFirst' left newg'
  where newg' = case left of 
                  Vertex a -> g'
                  -- When left is Vertex a, a is always a topo-first
                  -- node even it exists in right, which indicates a
                  -- self-looping edge a→a
                  otherwise -> (Connect g' right)

-- | topoFoldl tries to fold a graph in topological order. When there
-- is a edge x→y in the graph, x is always folded before y. If x and y
-- are not connected by any path, which node is folded first is
-- undetermined. topoFoldl stops when no node can be folded in the
-- graph and returns the folded value and the sub-graph of unfolded
-- nodes.
--
-- If the graph is acyclic, all nodes will be folded and an empty
-- graph is returned. For example, for a graph g as below:
--
-- 1→2→3
--   ↑
--   4
--
-- topoFoldl (flip (:)) [] g may return ([3, 2, 4, 1], empty) or ([3,
-- 2, 1, 4], empty), depending on the detail algebraic structure of g.
--
-- If the graph is cyclic, all foldable nodes are folded and the
-- unfoldable part of the graph is returned. For example, for a graph
-- g as below:
--
-- 1→2→3→6
--   ↑ ↓
--   4←5
--
-- topoFoldl (flip (:)) [] g returns ([1], g'), where g' is the
-- sub-graph of g consisting node 2, 3, 4, 5 and 6. Note that not only
-- those nodes forming cycles are unfoldable, but also those "blocked"
-- by cycles, like 6 in the above example.
topoFoldl :: Eq a => (b -> a -> b) -> b -> Graph a -> (b, Graph a)
topoFoldl f b Empty = (b, Empty)
topoFoldl f b (Vertex a) = ((f b a), Empty)
topoFoldl f b g =
  case findTopoFirst g of
    Just a -> topoFoldl f (f b a) (removeVertex a g)
    Nothing -> (b, g)
