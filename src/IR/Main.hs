import System.Environment (getArgs)
import System.Directory (doesFileExist)
import SExpTokens (alexScanTokens)
import SExpSyntax (parse)
import Eval (runProg)
import Env (emptyEnv)
import Heap (emptyHeap)
import Closure (clExp)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: ir script_file_name"
    (fname:_) -> do
      fexists <- doesFileExist fname
      case fexists of
        False -> putStrLn ("File " ++ fname ++ " doesn't exists.")
        True -> do contents <- readFile fname
                   let result = runProg emptyEnv emptyHeap $ parse $ alexScanTokens contents
                   case result of
                     Right cl -> putStrLn $ show $ clExp cl
                     Left err -> putStrLn $ "Evaluation failed. " ++ err


  
