module Syn (toTidy) where

import GHC
import DynFlags
import GHC.Paths (libdir)
import HscTypes
import SimplCore

ghcFrontEnd :: String -> Ghc (HscEnv, ModGuts)
ghcFrontEnd file
  = do getSessionDynFlags >>= setSessionDynFlags
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
  core2core ssn d
