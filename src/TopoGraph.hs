module TopoGraph (isTopoFree, findTopoFirst) where

import Algebra.Graph (Graph(..), hasVertex)

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
findTopoFirst' (Connect left right) g' = findTopoFirst' left (Connect g' right)

-- -- | topoFold tries to fold a graph in topological order. When there
-- -- is a edge x→y in the graph, x is always folded before y. If x
-- -- and y are not connected by any path, which node is folded first is
-- -- undetermined. topoFold stops when no node can be folded in the
-- -- graph and returns the folded value and the sub-graph of unfolded
-- -- nodes.
-- --
-- -- If the graph is acyclic, all nodes will be folded and an empty
-- -- graph is returned. For example, for a graph g as below:
-- --
-- -- 1→2→3
-- --   ↑
-- --   4
-- --
-- -- topoFold [] (:) g may return ([1, 4, 2, 3], empty) or ([4, 1, 2,
-- -- 3], empty), depending on the detail algebraic structure of g.
-- --
-- -- If the graph is cyclic, all foldable nodes are folded and the
-- -- unfoldable part of the graph is returned. For example, for a graph
-- -- g as below:
-- --
-- -- 1→2→3→6
-- --   ↑ ↓
-- --   4←5
-- --
-- -- topoFold [] (:) g returns ([1], g'), where g' is the sub-graph of g
-- -- consisting node 2, 3, 4, 5 and 6. Note that not only those nodes
-- -- forming cycles are unfoldable, but also those "blocked" by cycles,
-- -- like 6 in the above example.
-- topoFold :: Eq a => b -> (a -> b -> b) -> Graph a -> (b, Graph a)
-- topoFold b _ Empty -> (b, Empty)
-- topoFold b f (Vertex a) -> (f a b, Empty)

