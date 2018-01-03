module DFG where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Unique

import Data.Data

type NodeLabel = Int
type SignalMap = Map.Map Signal (Maybe NodeLabel, [NodeLabel])
data Graph = Graph { graphName :: String
                   , graphSignals :: SignalMap
                   , graphNodes :: Map.Map NodeLabel Node
                   , graphOutputs :: Set.Set Signal
                   }
            deriving (Show)

data Signal = Signal { signalID :: Either String Int
                     -- ^ String for a named signal, Int for an
                     -- anonymous one.
                     , signalWidth :: Int
                     }
            deriving (Show)

instance Eq Signal where
  a == b = (signalID a == signalID b) && (signalWidth a == signalWidth b)

instance Ord Signal where
  compare a b = case compare (signalID a) (signalID b) of
                  LT -> LT
                  GT -> GT
                  EQ -> compare (signalWidth a) (signalWidth b)

data BinOp = LessThan | GreaterThan | Minus | Plus deriving (Show)

data Node = CaseNode { caseOutput :: Signal
                     , caseCond :: Signal
                     , caseDefault :: Signal
                     , caseBranches :: [(Integer, Signal)]
                     }
          | BinNode { binOutput :: Signal
                    , op :: BinOp
                    , lhs :: Signal
                    , rhs :: Signal
                    }
          deriving (Show)

-- | @emptyGraph s@ creates a graph named @s@ with no node or signal.
emptyGraph :: String -> Graph 
emptyGraph s = Graph s Map.empty Map.empty Set.empty

type GraphS = State Graph
insertSignal :: Signal -> GraphS Signal
insertSignal s = do g <- get
                    put $ Graph
                            (graphName g)
                            (Map.insertWith (\ _ n -> n) s (Nothing, []) (graphSignals g))
                            (graphNodes g)
                            (graphOutputs g)
                    return s

insertOutputSignal :: Signal -> GraphS ()
insertOutputSignal s = do g <- get
                          put $ Graph
                                  (graphName g)
                                  (Map.insertWith (\ _ n -> n) s (Nothing, []) (graphSignals g))
                                  (graphNodes g)
                                  (Set.insert s $ graphOutputs g)

insertNode :: Node -> GraphS ()
insertNode n = do g <- get
                  let nl = Map.size $ graphNodes g
                  put $ Graph
                          (graphName g)
                          (connectNode n nl $ graphSignals g)
                          (Map.insert nl n $ graphNodes g)
                          (graphOutputs g)

autoSignal :: Maybe Signal -> Int -> GraphS Signal
autoSignal (Just s) _ = return s
autoSignal Nothing w = newSignal w

newSignal :: Int -> GraphS Signal
newSignal w = do g <- get
                 let s = Signal (Right $ Map.size $ graphSignals g) w
                 insertSignal s
                 return s

addOutput :: NodeLabel -> Signal -> SignalMap -> SignalMap
addOutput nl s smap = let (input, outputs) = smap Map.! s
                      in if elem nl outputs
                         then smap
                         else Map.insert s (input, nl:outputs) smap

addOutputToMany :: NodeLabel -> [Signal] -> SignalMap -> SignalMap
addOutputToMany nl ss smap = foldl (\smap' s -> addOutput nl s smap') smap ss

setInput :: NodeLabel -> Signal -> SignalMap -> SignalMap
setInput nl s smap = let (input, outputs) = smap Map.! s
                     in if input == Just nl
                        then smap
                        else Map.insert s (Just nl, outputs) smap

connectNode :: Node -> NodeLabel -> SignalMap -> SignalMap
connectNode (CaseNode o cond dflt branches) nl =
    (setInput nl o) . (addOutput nl cond) . (addOutputToMany nl (map snd branches))
connectNode (BinNode o op lhs rhs ) nl =
    (setInput nl o) . (addOutput nl lhs) . (addOutput nl rhs)

signalSource :: Signal -> Graph -> Maybe (NodeLabel, Node)
signalSource sig g = let (s, _) = graphSignals g Map.! sig
                     in case s of
                          Just nl -> Just (nl, graphNodes g Map.! nl)
                          Nothing -> Nothing
    

graphInputs :: Graph -> [Signal]
graphInputs g = let f s (i, _) is = case i of -- Check signal driver.
                                 Nothing -> s:is -- No driver, signal is an input.
                                 _ -> is         -- Not an input.
                in Map.foldrWithKey f [] $ graphSignals g

