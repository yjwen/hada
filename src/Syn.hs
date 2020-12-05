module Syn (toTidy, syn, collectInputVars) where

import GHC
import DynFlags
import GHC.Paths (libdir)
import HscTypes
import DynFlags (updOptLevel)
import HscTypes
import Outputable
import InstEnv (instEnvElts)
import SimplCore
import CoreSyn
import Var (Var, varName)
import Name (getOccString)
import CoreArity (etaExpand, exprArity)

import Util (varIsInModule)

ghcFrontEnd :: String -> Ghc (HscEnv, ModGuts)
ghcFrontEnd file
  = do dflags <- getSessionDynFlags
       -- Force optlevel = 1 to enable specialization in core
       -- optimization
       setSessionDynFlags $ updOptLevel 1 dflags
       -- To init package database
       target <- guessTarget file Nothing
       setTargets [target]
       load LoadAllTargets
       ssn <- getSession
       d <- getModuleGraph >>=
            (parseModule . head . mgModSummaries) >>=
            typecheckModule >>=
            desugarModule
       return (ssn, coreModule d)

toTidy :: String -> IO ModGuts
toTidy file = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  (ssn, d) <- runGhc (Just libdir) $ ghcFrontEnd file
  putStrLn $ showSDocUnsafe $ vcat $ map ppr (instEnvElts $ mg_inst_env d)
  core2core ssn d


-- If the var is TrNameS defined in GHC.Types
isGHCTypesTrNameSApp :: CoreExpr -> Bool
isGHCTypesTrNameSApp (App (Var v) _) =
  (getOccString v == "TrNameS") && (varIsInModule v "GHC.Types")
isGHCTypesTrNameSApp _ = False

-- If the var is Module define in GHC.Types
isGHCTypesModuleApp :: CoreExpr -> Bool
isGHCTypesModuleApp (App (App (Var v) _) _) =
  (getOccString v == "Module") && (varIsInModule v "GHC.Types")
isGHCTypesModuleApp _ = False


-- Select the core bind for implementing the top module. If top module
-- name is empty, select the first core bind with expression
syn :: String -> [CoreBind] -> CoreBind
syn t ((NonRec b e):binds)
  | isGHCTypesTrNameSApp e = goOn
  | isGHCTypesModuleApp e = goOn
  | otherwise = if (t == "") ||
                   getOccString (varName b) == t
                then NonRec b $ etaAbstraction e
                else goOn
  where goOn = syn t binds
syn t ((Rec bs):_) = error "Cannot synthesize recursive binding yet."
syn t [] = error ("Cannot find " ++ (if t == ""
                                     then "any top module"
                                     else "the top module " ++ t))

etaAbstraction :: CoreExpr -> CoreExpr
etaAbstraction e = etaExpand (exprArity e) e

-- | Assuming the expression being η-abstracted.
collectInputVars :: CoreExpr -> [Var]
collectInputVars (Lam b e) = b:collectInputVars e
collectInputVars _ = []
