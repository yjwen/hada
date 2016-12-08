import System.Environment (getArgs)
import GHC
import GHC.Paths (libdir)
import DynFlags
import HscTypes
import Outputable

import Verilog

main :: IO ()
main = do
  args <- getArgs
  result <- test $ head args
  putStrLn result 

filterTheNothing :: [Maybe a] -> [a]
filterTheNothing [] = []
filterTheNothing (x:xs) = case x of
                            Just a -> a:filterTheNothing xs
                            Nothing -> filterTheNothing xs

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
      let core = mg_binds $ coreModule d
      return $ showPpr dflags $ filterTheNothing $ map coreToVerilog core
  
