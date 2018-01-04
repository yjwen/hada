module Shell(Args(Args), synToVerilog) where

import GHC
import GHC.Paths (libdir)
import DynFlags
import HscTypes
import Outputable
import PprCore

import DFGSyn
import Verilog
import PprDFG

data Args = Args
    { dumpCore :: Bool
    , targetFile :: String
    }

prettyExcept :: Outputable a => (a -> SDoc) -> Either String (Maybe a) -> SDoc
prettyExcept _ (Left msg) = text $ "Error: " ++ msg
prettyExcept _ (Right Nothing) = empty
prettyExcept f (Right (Just a)) = f a

synToVerilog :: Args -> IO String
synToVerilog args =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags -- To init package database.
      target <- guessTarget (targetFile args) Nothing
      setTargets [target]
      load LoadAllTargets
           -- modSum <- getModSummary $ mkModuleName "Abs"
      modSums <- getModuleGraph
      p <- parseModule $ head modSums
      t <- typecheckModule p
      d <- desugarModule t
      let coreBinds = mg_binds $ coreModule d
          graphs = map bind coreBinds
          dumped = if dumpCore args
                   then (map ppr coreBinds) ++ (map (prettyExcept ppr) graphs)
                   else []
          vmodules = map (prettyExcept toVModule) graphs
      -- return $ showSDocUnsafe $ vcat $ (map ppr coreBinds) ++ dumped
      return $ showSDocUnsafe $ vcat $ dumped ++ vmodules ++ [text ""]
