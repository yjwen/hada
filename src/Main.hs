import System.Environment (getArgs)

import Shell
import Text.ParserCombinators.Parsec
import Data.Maybe (isJust, catMaybes)
import Data.List (intercalate)    

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
    Right args -> do result <- synToVerilog args
                     putStrLn result
