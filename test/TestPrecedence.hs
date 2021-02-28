import Test.HUnit
import Precedence
import Test

import System.Exit

import qualified WnegSum
import qualified WmulSum
import qualified WsumSub
import qualified WsumSum
import qualified WeqBand
import qualified WbandXor
import qualified WxorBor

allTests = (TestList . map (\(name, test) -> TestLabel name $ repeatedTest 1024 test))
           [ ("negSum", test2 negSum WnegSum.negSum)
           , ("mulSum", test3 mulSum WmulSum.mulSum)
           , ("sumSub", test3 sumSub WsumSub.sumSub)
           , ("sumSum", test3 sumSum WsumSum.sumSum)
           , ("eqBand", test3 eqBand WeqBand.eqBand)
           , ("bandXor", test3 bandXor WbandXor.bandXor)
           , ("xorBor", test3 xorBor WxorBor.xorBor)]

main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
