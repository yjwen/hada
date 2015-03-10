{-# LANGUAGE CPP #-}
import GHC
import Outputable
import GHC.Paths ( libdir )

       
import DynFlags

targetFile = "B.hs"

main :: IO ()
main = do
  res <- example
#if __GLASGOW_HASKELL__ > 704
  str <- runGhc (Just libdir) $ do
           dflags <- getSessionDynFlags
           return $ showSDoc dflags $ ppr res
  putStrLn str
#else
  putStrLn $ showSDoc ( ppr res )
#endif

example =
#if __GLASGOW_HASKELL__ > 704
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
#else
    defaultErrorHandler defaultLogAction $do
#endif
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                             [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "B"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d

        g <- getModuleGraph
        mapM showModule g
        return $ (parsedSource d, "/n-----/n", typecheckedSource d)
