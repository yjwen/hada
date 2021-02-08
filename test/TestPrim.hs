import Test.HUnit
import Prim
import Test

import System.Exit

import qualified WplusOne
import qualified WplusOne8
import qualified WplusOne16
import qualified WplusOne32
import qualified WplusOne64
import qualified WplusOneU
import qualified WplusOne8U
import qualified WplusOne16U
import qualified WplusOne32U
import qualified WplusOne64U

allTests = (TestList . map (\(name, test) -> TestLabel name $ repeatedTest 1024 test))
           [ ("plusOne", test1 plusOne WplusOne.plusOne)
           , ("plusOne8", test1 plusOne8 WplusOne8.plusOne8)
           , ("plusOne16", test1 plusOne16 WplusOne16.plusOne16)
           , ("plusOne32", test1 plusOne32 WplusOne32.plusOne32)
           , ("plusOne64", test1 plusOne64 WplusOne64.plusOne64)
           , ("plusOneU", test1 plusOneU WplusOneU.plusOneU)
           , ("plusOne8U", test1 plusOne8U WplusOne8U.plusOne8U)
           , ("plusOne16U", test1 plusOne16U WplusOne16U.plusOne16U)
           , ("plusOne32U", test1 plusOne32U WplusOne32U.plusOne32U)
           , ("plusOne64U", test1 plusOne64U WplusOne64U.plusOne64U)
           ]
main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
             
