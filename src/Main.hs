import System.Environment (getArgs)
import MyPpr
import HscTypes(mg_binds)
import Syn
import Verilog
import Outputable
import DFGSyn
import PprDFG
import Text.Parsec
import Data.Maybe (isJust, catMaybes)
import Data.List (intercalate)
import Control.Monad (liftM)
import Control.Monad.Trans.Except (runExcept)


data Action = DumpGraph | DumpCore | MyDumpCore
data Args = Args
    { action :: Action
    , targetFile :: String
    }

argSyntax = do
  dumpCore <- optionMaybe $ do {string "-";
                                choice [string "d", string "e"]}
  spaces
  fname <- many $ noneOf " \t\n"
  let action = case dumpCore of
                 Nothing -> DumpGraph
                 Just s -> if s == "d"
                           then DumpCore
                           else MyDumpCore

  return $ Args action fname
parseArgs :: String -> Either ParseError Args
parseArgs = parse argSyntax ""


prettyExcept :: Outputable a => (a -> SDoc) -> Either String (Maybe a) -> SDoc
prettyExcept _ (Left msg) = text $ "Error: " ++ msg
prettyExcept _ (Right Nothing) = empty
prettyExcept f (Right (Just a)) = f a

main :: IO ()
main = do
  args <- getArgs
  case parseArgs $ intercalate " " args of
    Left err -> putStrLn $ show err
    Right args -> do
      tidy <- toTidy $ targetFile args
      sdoc <- case action args of
                   DumpCore -> return $ vcat $ map ppr $ mg_binds tidy
                   MyDumpCore -> return $ myPpr tidy
                   otherwise -> do docs <- (mapM toVerilog $ mg_binds tidy)
                                   return $ vcat docs
      putStrLn $ showSDocUnsafe sdoc

