module TopoGraph ( isTopoFirst, isTopoLast
                 , findTopoFirst, findTopoLast
                 , findTopoFirstTrue, findTopoLastTrue
                 , topoFoldl, topoFoldr) where

import Algebra.Graph (Graph(..), hasVertex, removeVertex)

-- | isTopoFirst x g == true if there is no edge y→x (y /= x) exists in
-- graph g. Note any node that is not a node in g is a topo-free node
-- to g. And a node x∈g that has only incoming edge x→x is also
-- topo-free to g
isTopoFirst :: (Eq a) => a -> Graph a -> Bool
isTopoFirst a Empty = True
isTopoFirst a (Vertex b) = True
isTopoFirst a (Overlay left  right) = isTopoFirst a left && isTopoFirst a right
isTopoFirst a (Connect left Empty) = isTopoFirst a left
isTopoFirst a (Connect left right) = isTopoFirst a left && (not $ hasVertex a right)

-- | isTopoLast x g == true if there is no edge x→y (x /= y) exists in
-- graph g.
isTopoLast :: (Eq a) => a -> Graph a -> Bool
isTopoLast _ Empty = True
isTopoLast _ (Vertex _) = True
isTopoLast a (Overlay left right) = isTopoLast a left && isTopoLast a right
isTopoLast a (Connect Empty right) = isTopoLast a right
isTopoLast a (Connect left right) = isTopoLast a right && (not $ hasVertex a left)

-- | findTopoFirst g tries to find a node x∈g that is topo-first to g.
-- If no such node is found, return Nothing, otherwise return Just x.
findTopoFirst :: (Eq a) => Graph a -> Maybe a
findTopoFirst = findTopoFirstTrue (const True)

-- | findTopoFirstTrue f g tries to find a node x∈g that is topo-first
-- to g and f x == True.  If no such node is found, return Nothing,
-- otherwise return Just x.
findTopoFirstTrue :: (Eq a) => (a -> Bool) -> Graph a -> Maybe a
findTopoFirstTrue f g = findTopoFirstTrue' f g Empty

-- | findTopoLast g tries to find a node x∈g that is topo-last to
-- g. If no such node is found, return Nothing, otherwise return Just
-- x
findTopoLast :: (Eq a) => Graph a -> Maybe a
findTopoLast = findTopoLastTrue (const True)

-- | findTopoLastTrue f g tries to find a node x∈g that is topo-last
-- to g and f x == True. If no such node is found, return Nothing,
-- otherwise return Just x.
findTopoLastTrue :: (Eq a) => (a -> Bool) -> Graph a -> Maybe a
findTopoLastTrue f g = findTopoLastTrue' f g Empty

-- | findTopoFirstTrue' f g g' tries to find a node x∈g that is
-- topo-first to both g and g', and f x == True. If no such node is
-- found, return Nothing, otherwise return Just x
findTopoFirstTrue' :: (Eq a) => (a-> Bool) -> Graph a -> Graph a -> Maybe a
findTopoFirstTrue' _ Empty _ = Nothing
findTopoFirstTrue' f (Vertex b) g' = if isTopoFirst b g' && f b then Just b else Nothing
findTopoFirstTrue' f (Overlay left right) g' =
  case findTopoFirstTrue' f left (Overlay right g') of
    Just a -> Just a
    Nothing -> findTopoFirstTrue' f right (Overlay left g')
findTopoFirstTrue' f (Connect left right) g' =  findTopoFirstTrue' f left newg'
  where newg' = case left of 
                  Vertex a -> g'
                  -- When left is Vertex a, a is always a topo-first
                  -- node even it exists in right, which indicates a
                  -- self-looping edge a→a
                  otherwise -> (Connect g' right)
-- | findTopoLastTrue' f g g' tries to find a node x∈g that is topo-last to
-- both g and g', and f x == True.
findTopoLastTrue' :: (Eq a) => (a -> Bool) -> Graph a -> Graph a -> Maybe a
findTopoLastTrue' f Empty _ = Nothing
findTopoLastTrue' f (Vertex b) g' = if isTopoLast b g' && f b then Just b else Nothing
findTopoLastTrue' f (Overlay left right) g' =
  case findTopoLastTrue' f left (Overlay right g') of
    Just a -> Just a
    Nothing -> findTopoLastTrue' f right (Overlay left g')
findTopoLastTrue' f (Connect left right) g' = findTopoLastTrue' f right newg'
  where newg' = case right of
                  Vertex a -> g'
                  otherwise -> (Connect left g')

-- | topoFoldl tries to fold a graph in topological order. When there
-- is an edge x→y in the graph, x is always folded before y. If x and
-- y are not connected by any path, which node is folded first is
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

-- | topoFoldr tries to fold a graph in reversed topological
-- order. When there is an edge x→y in the graph, y is always folded
-- before x. If x and y are not connected by any path, which node is
-- folded first is undetermined. topoFoldr stops when no node can be
-- folded in the graph and returns the folded value and the sub-graph
-- of unfolded nodes.
topoFoldr :: Eq a => (a -> b -> b) -> b -> Graph a -> (b, Graph a)
topoFoldr f b Empty = (b, Empty)
topoFoldr f b (Vertex a) = ((f a b), Empty)
topoFoldr f b g =
  case findTopoLast g of
    Just a -> topoFoldr f (f a b) (removeVertex a g)
    Nothing -> (b, g)
