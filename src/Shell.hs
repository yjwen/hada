module Shell(Action(DumpGraph, DumpCore, Experiment), Args(Args), synToVerilog) where

import GHC
import GHC.Paths (libdir)
import DynFlags
import HscTypes
import Outputable
import PprCore
import SimplCore

import Syn
import DFGSyn
import Verilog
import PprDFG

import Control.Monad.Trans.Except (runExcept)

prettyExcept :: Outputable a => (a -> SDoc) -> Either String (Maybe a) -> SDoc
prettyExcept _ (Left msg) = text $ "Error: " ++ msg
prettyExcept _ (Right Nothing) = empty
prettyExcept f (Right (Just a)) = f a

synToVerilog :: Args -> IO String
synToVerilog args = do tidy <- toTidy $ targetFile args
                       let graphs = map (runExcept . bind) $ mg_binds tidy
                           vmodules = map (prettyExcept toVModule) graphs
                       return $ showSDocUnsafe $ vcat $
                         case action args of
                           DumpCore -> map ppr $ mg_binds tidy
                           otherwise -> map (prettyExcept toVModule) graphs

