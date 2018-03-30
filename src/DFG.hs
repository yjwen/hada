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
data SignalType = SimpleSigType Int
                | AlgebraicType { algSigName :: [String]  -- Can be hierarchical
                                , algSigType :: Int
                                , algSigData :: Int
                                }
                deriving (Show, Eq, Ord)

typeWidth :: SignalType -> Int
typeWidth (SimpleSigType w) = w
typeWidth (AlgebraicType _ t d) = t + d

data Signal = Signal { signalID :: Either String Int
                     -- ^ String for a named signal, Int for an
                     -- anonymous one.
                     , signalType :: SignalType
                     }
            deriving (Show, Eq, Ord)

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

autoSignal :: Maybe Signal -> SignalType -> GraphS Signal
autoSignal (Just s) _ = return s
autoSignal Nothing t = newSignal t

newSignal :: SignalType -> GraphS Signal
newSignal t = do g <- get
                 let s = Signal (Right $ Map.size $ graphSignals g) t
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

