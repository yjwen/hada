module DFG where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified CoreSyn as C
import Control.Monad.Trans.State
import Var
import Name
import Type
import TyCon
import DataCon
import Data.Data
import Data.Bits
import Outputable

type NodeLabel = Int

data Graph = Graph { graphName :: String
                   , graphSignals :: Map.Map Signal (Maybe NodeLabel, [NodeLabel])
                   , graphNodes :: Map.Map NodeLabel Node
                   , graphOutputs :: Set.Set Signal
                   }
            deriving (Show)

data Signal = Signal { signalID :: Either String Int
                     -- ^ String for a named signal, Int for an
                     -- anonymous one.
                     , signalWidth :: Maybe Int
                     }
            deriving (Show)

instance Eq Signal where
  a == b = (signalID a == signalID b) && (signalWidth a == signalWidth b)

instance Ord Signal where
  compare a b = case compare (signalID a) (signalID b) of
                  LT -> LT
                  GT -> GT
                  EQ -> compare (signalWidth a) (signalWidth b)

data Node = CaseNode { caseOutput :: Signal
                     , caseCond :: Signal
                     , caseBranches :: [(Int, Signal)]
                     }
          deriving (Show)

-- | @emptyGraph s@ creates a graph named @s@ with no node or signal.
emptyGraph :: String -> Graph 
emptyGraph s = Graph s Map.empty Map.empty Set.empty

type GraphS = State Graph
insertSignal :: Signal -> GraphS ()
insertSignal s = do g <- get
                    put $ Graph
                            (graphName g)
                            (Map.insertWith (\ _ n -> n) s (Nothing, []) (graphSignals g))
                            (graphNodes g)
                            (graphOutputs g)

insertOutputSignal :: Signal -> GraphS ()
insertOutputSignal s = do g <- get
                          put $ Graph
                                  (graphName g)
                                  (Map.insertWith (\ _ n -> n) s (Nothing, []) (graphSignals g))
                                  (graphNodes g)
                                  (Set.insert s $ graphOutputs g)

graphInputs :: Graph -> [Signal]
graphInputs g = let f s (i, _) is = case i of -- Check signal driver.
                                 Nothing -> s:is -- No driver, signal is an input.
                                 _ -> is         -- Not an input.
                in Map.foldrWithKey f [] $ graphSignals g

translateBind :: C.CoreBind -> Maybe Graph
translateBind (C.NonRec b e) =
  let (_, outputType) = splitFunTys $ varType b
      (inputVars, decurriedExp) = decurry e
      toSignal v = mkSignal (Var.varType v) (getOccString $ Var.varName v)
      moduleName = getOccString b
      out = mkSignal outputType "out"
      (_, g) = runState (insertOutputSignal out) $ emptyGraph moduleName
  in case head moduleName of
    '$' -> Nothing
    otherwise -> Just g

translateBind (C.Rec _) = error "Cannot translate C.Rec"

decurry :: C.Expr a -> ([a], C.Expr a)
decurry (C.Lam v e) = (v:vs, dexp)
  where (vs, dexp) = decurry e
decurry e = ([], e)

mkSignal :: Type -> String -> Signal
mkSignal t n = case getTypeBits t of
                 Just r -> if r == 1
                           then Signal (Left n) Nothing
                           else Signal (Left n) (Just r)
                 otherwise -> error "Unknown type for getTypeBits"


getTypeBits :: Type -> Maybe Int
getTypeBits t = case splitTyConApp_maybe t of
                  Just (tyCon, args) ->
                    if isAlgTyCon tyCon
                    then case getOccString $ getName tyCon of
                      "Word" -> Just $ finiteBitSize (0::Word)
                      otherwise -> error "Unsupported type"
                    else error "Non-algegra type"
                  Nothing -> error "splitTyConApp_maybe failed"

