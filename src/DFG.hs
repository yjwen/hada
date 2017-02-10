module DFG where

import qualified Data.Map.Strict as Map
import qualified CoreSyn as C
import Var
import Name
import Type
import TyCon
import DataCon
import Data.Data
import Data.Bits

newtype NodeLabel = Int
newtype SignalLabel = Int

data Graph = Graph { graphName :: String
                   , graphSignals :: Map.Map SignalLabel Signal
                   , graphNodes :: Map.Map NodeLabel Node
                   , graphSignalNames :: Map.Map Signal SignalLabel
                   }
            deriving (Show)

data Signal = Signal { signalID :: Either String Int
                     -- ^ String for a named signal, Int for an
                     -- anonymous one.
                     , signalWidth :: Maybe Int
                     , signalDriver :: NodeLabel
                     , signalSinks :: [NodeLabel]
                     }
            deriving (Show)

instance Eq Signal where
  a == b = (signalID a == signalID b) && (signalWidth a == signalWidth b)

instance Ord Signal where
  compare a b = case compare (signalID a) (signalID b) of
                  LT -> LT
                  GT -> GT
                  EQ -> compare (signalWidth a) (signalWidth b)

data Node = CaseNode { caseOutput :: SignalLabel
                     , caseCond :: SignalLabel
                     , caseBranches :: [(Int, SignalLabel)]
                     }
          deriving (Show)

-- | @initGraph s@ creates a graph named @s@ with no node or signal.
initGraph :: String -> Graph 
initGraph inputs output = Graph s Map.empty Map.empty Map.empty

translateBind :: C.CoreBind -> Maybe Graph
translateBind (C.NonRec b e) =
  let (_, outputType) = splitFunTys $ varType b
      (inputVars, decurriedExp) = decurry e
      toSignal v = mkSignal (Var.varType v) (getOccString $ Var.varName v)
      moduleName = getOccString b
  in case head moduleName of
    '$' -> Nothing
    otherwise -> Just $ Graph moduleName (map toSignal inputVars) [(mkSignal outputType "out")]

translateBind (C.Rec _) = error "Cannot translate C.Rec"

decurry :: C.Expr a -> ([a], C.Expr a)
decurry (C.Lam v e) = (v:vs, dexp)
  where (vs, dexp) = decurry e
decurry e = ([], e)

mkSignal :: Type -> String -> Signal
mkSignal t n = case getTypeBits t of
                 Just r -> if r == 1
                           then Signal (Just n) Nothing []
                           else Signal (Just n) (Just r) []
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

