import System.Environment (getArgs)
import GHC
import GHC.Paths (libdir)
import DynFlags
import HscTypes
import Outputable

main :: IO ()
main = do
  args <- getArgs
  result <- test $ head args
  putStrLn result 
  -- res <- printCore $ head args
  -- str <- runGhc (Just libdir) $ do
  --          dflags <- getSessionDynFlags
  --          return $ showSDoc dflags $ ppr res
  -- putStrLn str

test :: String -> IO String
test targetFile =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags -- To init package database.
      target <- guessTarget targetFile Nothing
      setTargets [target]
      load LoadAllTargets
      modSum <- getModSummary $ mkModuleName "Abs"
      p <- parseModule modSum
      t <- typecheckModule p
      d <- desugarModule t
      return $ showSDoc dflags $ ppr $ (parsedSource d, typecheckedSource d)
  
readCoreModule :: DesugaredModule -> String
readCoreModule d = let core = dm_core_module d
                   in moduleNameString $ moduleName $ mg_module core
