import System.Environment (getArgs)
import GHC
import GHC.Paths (libdir)
import DynFlags
import HscTypes
import Outputable
import PprCore

import DFGSyn
import Verilog
import PprDFG

import Text.ParserCombinators.Parsec

import Data.Maybe (isJust, catMaybes)
import Data.List (intercalate)    
data Args = Args
    { dumpCore :: Bool
    , targetFile :: String
    }

argSyntax = do
  opt <- optionMaybe $ string "-d"
  spaces
  fname <- many $ noneOf " \t\n"
  return $ Args (isJust opt) fname
parseArgs :: String -> Either ParseError Args
parseArgs = parse argSyntax ""
          
main :: IO ()
main = do
  args <- getArgs
  case parseArgs $ intercalate " " args of
    Left err -> putStrLn $ show err
    Right args -> do result <- test args
                     putStrLn result

prettyExcept :: Outputable a => (a -> SDoc) -> Either String a -> SDoc
prettyExcept _ (Left msg) = text $ "Error: " ++ msg
prettyExcept f (Right a) = f a

test :: Args -> IO String
test args =
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
          graphs = map translateBind coreBinds
          dumped = if dumpCore args
                   then (map ppr coreBinds) ++ (map (prettyExcept ppr) graphs)
                   else []
          vmodules = map (prettyExcept toVModule) graphs
      -- return $ showSDocUnsafe $ vcat $ (map ppr coreBinds) ++ dumped
      return $ showSDocUnsafe $ vcat $ dumped ++ vmodules
  
