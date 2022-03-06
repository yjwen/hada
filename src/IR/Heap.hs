module Heap (Heap, access, insertHeap, size, newAlloc, emptyHeap) where

import Closure(Closure)

import Data.IntMap(IntMap, (!?), insert, size, empty)

type Heap = IntMap Closure

-- Another name of (!?), to avoid name conflicts
access :: Heap -> Int -> Maybe Closure
access = (!?)

-- Another name of insert, to avoid name conflicts
insertHeap = insert

newAlloc :: Closure -> Heap -> (Heap, Int)
newAlloc c h = let addr = size h
               in (insertHeap addr c h, addr)

emptyHeap :: Heap
emptyHeap = empty
