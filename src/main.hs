import System.Environment (getArgs)
import Parse (printCore)
import GHC
import GHC.Paths ( libdir )
import DynFlags
import Outputable

main :: IO ()
main = do
  args <- getArgs
  res <- printCore $ head args
  str <- runGhc (Just libdir) $ do
           dflags <- getSessionDynFlags
           return $ showSDoc dflags $ ppr res
  putStrLn str
