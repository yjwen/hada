import System.Directory (listDirectory)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import SExpTokens(alexScanTokens)
import SExpSyntax(parse)
import Syntax(Exp(..))
import Eval(runProg)
import Heap(emptyHeap)
import Env(emptyEnv)
import Closure(Closure(Closure), clExp)
import Data.List (isSuffixOf)

testDir = "test/IR/"

listIRFiles :: IO [String]
listIRFiles = do {
  files <- listDirectory testDir;
  return $ filter (isSuffixOf ".ir") files;
  }
                 

testIR :: String -> Test
testIR fname = TestLabel fname $ TestCase (
  do contents <- readFile (testDir ++ fname)
     let r = runProg emptyEnv emptyHeap $ parse $ alexScanTokens contents
     case r of
       Right (Closure exp _) ->
         case exp of
           BoolLitExp True -> return ()
           otherwise -> assertFailure ("Evaluates to " ++ show exp)
       Left err -> assertFailure ("Evaluation failed. " ++ err)
  )


main :: IO ()
main = do files <- listIRFiles
          counts <- runTestTT $ TestList $ map testIR files
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
            
