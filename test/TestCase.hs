import Test.HUnit
import Case
import Test

import System.Exit

import qualified WandOrBool

allTests = TestList [TestLabel "andorBool" $ repeatedTest 8 $ test3 andOrBool WandOrBool.andOrBool]

main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess

