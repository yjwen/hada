module Closure (Closure(..), justExp) where

import Env(Env)
import Syntax(Exp(..))
import Data.Map.Strict(Map, empty, (!?))

-- A closure consists of an expression and an environment that maps
-- symbols to heap addresses
data Closure = Closure { clExp :: Exp
                       , clEnv :: Env
                       }
               
                        
justExp :: Exp -> Closure
justExp exp = Closure exp empty
