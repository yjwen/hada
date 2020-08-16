import System.Environment (getArgs)
import System.FilePath
import MyPpr
import HscTypes(mg_binds)
import Syn
import SVerilog
import Outputable
import DFGSyn
import PprDFG
import Text.ParserCombinators.ReadP
import Data.Maybe (isJust, catMaybes)
import Data.List (intercalate)
import Control.Monad (liftM)
import Control.Monad.Trans.Except (runExcept)


data Action = DumpCore | MyDumpCore
data OutputFile = DefaultOutF -- Output file name derived from target file
                | ExplicitOutF String -- Explicit output file
                | StandardOutF -- To standard output
data Param = Param { action :: Maybe Action
                   , outputFile :: OutputFile
                   , targetFile :: String
                   }

parseArgs :: [String] -> Param -> Either String Param
parseArgs (s:ss) p
  | s == "-e" = parseArgs ss $ p {action = Just MyDumpCore}
  | s == "-d" = parseArgs ss $ p {action = Just DumpCore}
  | s == "-o" = case ss of
                  (oname:remain_args) -> parseArgs remain_args $ p {outputFile = ExplicitOutF oname}
                  otherwise -> Left "Output file name is missing"
  | s == "-p" = parseArgs ss $ p {outputFile = StandardOutF}
  | otherwise = parseArgs ss $ p {targetFile = s}
parseArgs [] p = Right p

defaultParam :: Param
defaultParam = Param Nothing DefaultOutF ""

-- xxx.hs -> xxx.sv
getDefaultOutputFile :: String -> String
getDefaultOutputFile f = addExtension (dropExtension f) ".sv"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args defaultParam of
    Left err -> putStrLn $ show err
    Right param -> do
      tidy <- toTidy $ targetFile param
      -- Dump if required
      case action param of
        Just DumpCore -> putStrLn $ showSDocUnsafe $ vcat $ map ppr $ mg_binds tidy
        Just MyDumpCore -> putStrLn $ showSDocUnsafe $ myPpr tidy
        otherwise -> return ()
      -- Output result
      docs <- mapM toSV $ mg_binds tidy
      let doc_str = showSDocUnsafe $ vcat docs
      case outputFile param of
        StandardOutF -> putStrLn doc_str
        DefaultOutF -> writeFile (getDefaultOutputFile $ targetFile param) doc_str
        ExplicitOutF fname -> writeFile fname doc_str

