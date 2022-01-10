import System.Environment (getArgs)
import System.Directory (doesFileExist)
import SExpTokens (alexScanTokens)
import SExpSyntax (parse)
import Eval (runProg)
import Data.Map.Strict(empty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: ir script_file_name"
    (fname:_) -> do
      fexists <- doesFileExist fname
      case fexists of
        False -> putStrLn ("File " ++ fname ++ " doesn't exists.")
        True -> readFile fname >>= (putStrLn . show . runProg empty . parse . alexScanTokens)

  
