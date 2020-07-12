module Syn (toTidy) where

import GHC
import DynFlags
import GHC.Paths (libdir)
import HscTypes
import DynFlags (updOptLevel)
import HscTypes
import Outputable
import InstEnv (instEnvElts)
import SimplCore

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
            (parseModule . head) >>=
            typecheckModule >>=
            desugarModule
       return (ssn, coreModule d)

toTidy :: String -> IO ModGuts
toTidy file = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  (ssn, d) <- runGhc (Just libdir) $ ghcFrontEnd file
  putStrLn $ showSDocUnsafe $ vcat $ map ppr (instEnvElts $ mg_inst_env d)
  core2core ssn d
