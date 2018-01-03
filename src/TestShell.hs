module Main where

import Data.List.Split

import Test.HUnit
import System.Exit
import Shell

main = do cnts <- runTestTT $ TestCase $ compareExamples "abs"
          if failures cnts > 0
            then exitFailure
            else exitSuccess

compareExamples :: String -> IO ()
compareExamples fname = do g <- readFile $ "./examples/" ++ fname ++ ".v"
                           t <- synToVerilog $ Args False $ "./examples/" ++ fname ++ ".hs"
                           putStrLn g
                           putStrLn t
                           assertString $ compareByLines g t


compareLines :: [String] -> [String] -> String
compareLines a b = compareLines' (map trimLeft a) (map trimLeft b) 1
  where trimLeft = dropWhile (==' ')

compareLines' [] [] _ = ""
compareLines' [] (t:tx) l = "line " ++ (show l) ++ ": golden='', test='" ++ t ++ "'"
compareLines' (g:gx) [] l = "line " ++ (show l) ++ ": golden='" ++ g ++ "', test=''"
compareLines' (g:gx) (t:tx) l | t == g = compareLines' gx tx (l + 1)
                              | otherwise = "line = " ++ (show l) ++ ": golden='" ++ g ++ "', test='" ++ t ++ "'"

compareByLines :: String -> String -> String
compareByLines g t = compareLines (splitOn "\n" t) (splitOn "\n" t)



  
