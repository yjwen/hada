import System.Directory(listDirectory)
import System.Exit(exitFailure, exitSuccess)
import Test.HUnit

import qualified TypeTokens(alexScanTokens)
import qualified TypeSyntax(parse)
import qualified SExpTokens(alexScanTokens)
import qualified SExpSyntax(parse)

import InferType(runInfer, infer, InferState)
import Type(Type(TyAny))
import Syntax(progDefs, progExp, defSym, defExp, DefineStat)
import Data.Map.Lazy(empty, fromList)
import Data.List (isSuffixOf)

testDir = "test/IR/"

listFiles :: IO [String]
listFiles = do files <- listDirectory testDir
               return $ filter (isSuffixOf ".ty") files

initS :: [DefineStat] -> InferState
initS defs = ( empty -- Empty type map
             , fromList (map (\def -> (defSym def, defExp def)) defs) -- expression map
             )
              
testInfer :: String -> Test
testInfer fname = TestLabel fname $ TestCase (
  do contents <- readFile (testDir ++ fname)
     let (typepart, irpart) = break (=='\n') contents
         -- First line is type line, while remaining lines are program line
         t = TypeSyntax.parse $ TypeTokens.alexScanTokens typepart
         prog = SExpSyntax.parse $ SExpTokens.alexScanTokens irpart
     case (runInfer
           (initS $ progDefs prog) -- initial state
           (infer TyAny $ progExp prog) -- the job
          ) of
       Left err -> assertFailure ("Type inference failed: " ++ err)
       Right t' ->
         if t' == t
         then return ()
         else assertEqual "Incorrect type: " t t' )
                  
                  
main :: IO ()
main = do files <- listFiles
          counts <- runTestTT $ TestList $ map testInfer files
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
