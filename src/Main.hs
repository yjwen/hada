import System.Environment (getArgs)
import System.FilePath ((<.>), dropExtension, takeBaseName)
import MyPpr
import Verilator (cppDriver, hsWrapper)
import HscTypes(mg_binds)
import Syn (syn, toTidy)
import SVerilog
import Outputable
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
                   , topBind :: String
                   , wrapperFile :: String
                   }

parseArgs :: [String] -> Param -> Either String Param
parseArgs (s:ss) p
  -- For experiments, dumping the core
  | s == "-e" = parseArgs ss $ p {action = Just MyDumpCore}
  -- Dump the core
  | s == "-d" = parseArgs ss $ p {action = Just DumpCore}
  -- Write output to a file
  | s == "-o" = case ss of
                  (oname:remain_args) -> parseArgs remain_args $ p {outputFile = ExplicitOutF oname}
                  otherwise -> Left "Output file name is missing"
  -- Print output to standard output
  | s == "-p" = parseArgs ss $ p {outputFile = StandardOutF}
  -- Specify the top module
  | s == "-t" = case ss of
                  (name:remain_args) -> parseArgs remain_args $ p {topBind = name}
                  otherwise -> Left "Top bind name is missing"
  -- Base name for Verilator wrapper files
  | s == "-w" = case ss of
                  (name:remain_args) -> parseArgs remain_args $ p {wrapperFile = name}
                  otherwise -> Left "Wrapper file name is missing"
  -- Target file
  | otherwise = parseArgs ss $ p {targetFile = s}

parseArgs [] p = if (topBind p) == ""
                 then Left "No top bind is given. Must give a top bind by -t"
                 else Right p

defaultParam :: Param
defaultParam = Param Nothing DefaultOutF "" "" ""

-- xxx.hs -> xxx.sv
getDefaultOutputFile :: String -> String
getDefaultOutputFile f = (dropExtension f) <.> "sv"

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
      let top = syn (topBind param) $ mg_binds tidy
      (docs, vo) <- toSV top
      let doc_str = showSDocUnsafe docs
      case outputFile param of
        StandardOutF -> putStrLn doc_str
        DefaultOutF -> writeFile (getDefaultOutputFile $ targetFile param) doc_str
        ExplicitOutF fname -> writeFile fname doc_str
      let wName = wrapperFile param
      if wName == ""
        then return ()
        else do writeFile (wName <.> "cpp") (showSDocUnsafe $ cppDriver top vo)
                writeFile (wName <.> "hs") (showSDocUnsafe $ hsWrapper (takeBaseName wName) top vo)
