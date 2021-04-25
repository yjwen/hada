module TopoGraph (isTopoFirst) where

import Algebra.Graph (Graph(..), hasVertex)

-- | isTopoFirst x g == true if there is no edge y→x (y /= x) exists
-- in graph g. Note for x that is not a node in g, isTopoFirst x g ==
-- true. And for a node x that exists in g and has only incoming edge
-- x→x, isTopoFirst x g == true.
isTopoFirst :: (Eq a) => a -> Graph a -> Bool
isTopoFirst a Empty = True
isTopoFirst a (Vertex b) = True
isTopoFirst a (Overlay left  right) = isTopoFirst a left && isTopoFirst a right
isTopoFirst a (Connect left Empty) = isTopoFirst a left
isTopoFirst a (Connect left right) = isTopoFirst a left && (not $ hasVertex a right)

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

