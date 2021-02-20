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
import qualified WplusOneU8
import qualified WplusOneU16
import qualified WplusOneU32
import qualified WplusOneU64
import qualified Wlshc
import qualified Wlshc8
import qualified Wlshc16
import qualified Wlshc32
import qualified Wlshc64
import qualified WlshcU
import qualified WlshcU8
import qualified WlshcU16
import qualified WlshcU32
import qualified WlshcU64
import qualified Wrshc
import qualified Wrshc8
import qualified Wrshc16
import qualified Wrshc32
import qualified Wrshc64
import qualified WrshcU
import qualified WrshcU8
import qualified WrshcU16
import qualified WrshcU32
import qualified WrshcU64

allTests = (TestList . map (\(name, test) -> TestLabel name $ repeatedTest 1024 test))
           [ ("plusOne", test1 plusOne WplusOne.plusOne)
           , ("plusOne8", test1 plusOne8 WplusOne8.plusOne8)
           , ("plusOne16", test1 plusOne16 WplusOne16.plusOne16)
           , ("plusOne32", test1 plusOne32 WplusOne32.plusOne32)
           , ("plusOne64", test1 plusOne64 WplusOne64.plusOne64)
           , ("plusOneU", test1 plusOneU WplusOneU.plusOneU)
           , ("plusOneU8", test1 plusOneU8 WplusOneU8.plusOneU8)
           , ("plusOneU16", test1 plusOneU16 WplusOneU16.plusOneU16)
           , ("plusOneU32", test1 plusOneU32 WplusOneU32.plusOneU32)
           , ("plusOneU64", test1 plusOneU64 WplusOneU64.plusOneU64)
           , ("lshc", test1 lshc Wlshc.lshc)
           , ("lshc8", test1 lshc8 Wlshc8.lshc8)
           , ("lshc16", test1 lshc16 Wlshc16.lshc16)
           , ("lshc32", test1 lshc32 Wlshc32.lshc32)
           , ("lshc64", test1 lshc64 Wlshc64.lshc64)
           , ("lshcU", test1 lshcU WlshcU.lshcU)
           , ("lshcU8", test1 lshcU8 WlshcU8.lshcU8)
           , ("lshcU16", test1 lshcU16 WlshcU16.lshcU16)
           , ("lshcU32", test1 lshcU32 WlshcU32.lshcU32)
           , ("lshcU64", test1 lshcU64 WlshcU64.lshcU64)
           , ("rshc", test1 rshc Wrshc.rshc)
           , ("rshc8", test1 rshc8 Wrshc8.rshc8)
           , ("rshc16", test1 rshc16 Wrshc16.rshc16)
           , ("rshc32", test1 rshc32 Wrshc32.rshc32)
           , ("rshc64", test1 rshc64 Wrshc64.rshc64)
           , ("rshcU", test1 rshcU WrshcU.rshcU)
           , ("rshcU8", test1 rshcU8 WrshcU8.rshcU8)
           , ("rshcU16", test1 rshcU16 WrshcU16.rshcU16)
           , ("rshcU32", test1 rshcU32 WrshcU32.rshcU32)
           , ("rshcU64", test1 rshcU64 WrshcU64.rshcU64)
           ]
main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
             
