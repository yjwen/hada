module Env(Env, HeapAddr, symbolAddr, insertEnv, emptyEnv) where

import Data.Map.Lazy(Map, (!?), insert, empty)

type HeapAddr = Int -- As heap is modeled by a Data.IntMap, heap
                    -- address is just an Int

type Env = Map String HeapAddr
-- An environment maps symbols to heap addresses

symbolAddr :: Env -> String -> Maybe HeapAddr
symbolAddr = (!?)

insertEnv :: String -> HeapAddr -> Env -> Env
insertEnv = insert


emptyEnv :: Env
emptyEnv = empty
