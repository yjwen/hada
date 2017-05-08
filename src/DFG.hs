module DFG where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified CoreSyn as C
import Control.Monad.Trans.State
import Var
import Unique
import Name
import Literal
import Type
import TyCon
import DataCon
import Data.Data
import Data.Bits
import Outputable

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
                     , signalWidth :: Maybe Int
                     }
            deriving (Show)
signalBits :: Signal -> Int
signalBits s = case signalWidth s of
                 Just i -> i
                 Nothing -> 1

instance Eq Signal where
  a == b = (signalID a == signalID b) && (signalWidth a == signalWidth b)

instance Ord Signal where
  compare a b = case compare (signalID a) (signalID b) of
                  LT -> LT
                  GT -> GT
                  EQ -> compare (signalWidth a) (signalWidth b)

data Node = CaseNode { caseOutput :: Signal
                     , caseCond :: Signal
                     , caseDefault :: Signal
                     , caseBranches :: [(Integer, Signal)]
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

insertNode :: Node -> GraphS ()
insertNode n = do g <- get
                  let nl = Map.size $ graphNodes g
                  put $ Graph
                          (graphName g)
                          (connectNode n nl $ graphSignals g)
                          (Map.insert nl n $ graphNodes g)
                          (graphOutputs g)

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
    
showppr :: Outputable a => a -> String
showppr = showSDocUnsafe . ppr

synExp :: C.Expr Var -> Signal -> GraphS ()
synExp (C.Case e v t alts) s =
    do let vt = varType v
           cond = mkVarSignal v
       insertSignal cond
       insertSignal $ Signal (Left $ "v=" ++ showppr (Var.varName v) ++ (show $ isId v) ++ (show $ isTyVar v) ++ (show $ isTcTyVar v)) Nothing
       insertSignal $ Signal (Left $ "vt=" ++ showppr vt) Nothing
       insertSignal $ Signal (Left $ "t=" ++ showppr t) Nothing
       (dflt, branches) <- synAlt alts
       case dflt of
         Just dflt' -> insertNode (CaseNode s cond dflt' branches)
         Nothing -> if length branches == (1 `shift` signalBits cond)
                    then insertNode $ CaseNode s cond (snd $ head branches) (tail branches)
                    else error "Incomplete branches"
       return ()

synExp _ _ = error "Unknown expression"


synAlt :: [C.Alt Var] -> GraphS (Maybe Signal, [(Integer, Signal)])
synAlt [] = do return (Nothing, [])
synAlt ((altCon, vars, exp):alts) =
    do (dflt, branches) <- synAlt alts
       case altCon of
         C.LitAlt lit -> case lit of
                           MachInt i -> do let altsig = Signal (Left $ "alt" ++ show i) Nothing
                                           insertSignal altsig
                                           return (dflt, (i, altsig):branches)
                           _ -> error $ "Unsupported lit " ++ showppr lit
         C.DEFAULT -> do let dfltsig = Signal (Left $ "default_signal") Nothing
                         insertSignal dfltsig
                         return (Just dfltsig, branches)
         C.DataAlt dc -> do let tag = dataConTag dc
                                sig = Signal (Left $ "alt" ++ showppr dc ++ "_" ++ show tag) Nothing
                            insertSignal sig
                            return (dflt, (toInteger $ tag - 1, sig):branches)

graphInputs :: Graph -> [Signal]
graphInputs g = let f s (i, _) is = case i of -- Check signal driver.
                                 Nothing -> s:is -- No driver, signal is an input.
                                 _ -> is         -- Not an input.
                in Map.foldrWithKey f [] $ graphSignals g

translateBind :: C.CoreBind -> Maybe Graph
translateBind (C.NonRec b e) =
  let (_, outputType) = splitFunTys $ varType b
      (inputVars, decurriedExp) = decurry e
      moduleName = getOccString b
      out = mkSignal outputType "out"
      (_, g) = runState syn $ emptyGraph moduleName
      syn = do insertOutputSignal out
               synExp decurriedExp out
  in case head moduleName of
    '$' -> Nothing
    otherwise -> Just g

translateBind (C.Rec _) = error "Cannot translate C.Rec"

decurry :: C.Expr a -> ([a], C.Expr a)
decurry (C.Lam v e) = (v:vs, dexp)
  where (vs, dexp) = decurry e
decurry e = ([], e)

mkVarSignal :: Var -> Signal
mkVarSignal v = let n = if isId v
                        then Right $ getKey $ varUnique v
                        else Left $ getOccString $ Var.varName v
                in case getTypeBits (Var.varType v) of
                     Just r -> Signal n (if r == 1 then Nothing else Just r)
                     otherwise -> error "Unknown type for getTypeBits"

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
                      "Bool" -> Just 1
                      otherwise -> error "Unsupported type"
                    else error "Non-algegra type"
                  Nothing -> error "splitTyConApp_maybe failed"

