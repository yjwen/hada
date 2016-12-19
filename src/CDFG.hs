module CDFG where

import qualified CoreSyn as C
import Var
import Name
import Type
import TyCon
import DataCon
import Data.Data
import Data.Bits

data Graph = Graph { graphName :: String
                   , graphInputs :: [Signal]
                   , graphOutputs :: [Signal]
                   }
            deriving (Show)

data Signal = Signal { signalName :: Maybe String
                     , signalWidth :: Maybe Int
                     , signalPins :: [Pin]
                     }
              deriving (Show)

data PinDirection = Input | Output deriving (Show)

data Pin = Pin { pinName :: String
               , pinDirection :: PinDirection
               , pinSignal :: Signal
               , pinSignalSlicing :: Maybe (Int, Int)
               , pinNode :: Node
               }
           deriving (Show)

data Node = Node deriving (Show)


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
                 otherwise -> error "Unknow type for getTypeBits"


getTypeBits :: Type -> Maybe Int
getTypeBits t = case splitTyConApp_maybe t of
                  Just (tyCon, args) ->
                    if isAlgTyCon tyCon
                    then case getOccString $ getName tyCon of
                      "Word" -> Just $ finiteBitSize (0::Word)
                      otherwise -> error "Unsupported type"
                    else error "Non-algegra type"
                  Nothing -> error "splitTyConApp_maybe failed"

