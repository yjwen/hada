import Test.HUnit
import System.Random
import Control.Monad
import Num

import System.Exit

import qualified Wplus
import qualified WplusW
import qualified Wplus8
import qualified WplusW8
import qualified Wplus16
import qualified WplusW16
import qualified Wplus32
import qualified WplusW32
import qualified Wplus64
import qualified WplusW64
import qualified Wminus
import qualified WminusW
import qualified Wminus8
import qualified WminusW8
import qualified Wminus16
import qualified WminusW16
import qualified Wminus32
import qualified WminusW32
import qualified Wminus64
import qualified WminusW64
import qualified Wmul
import qualified WmulW
import qualified Wmul8
import qualified WmulW8
import qualified Wmul16
import qualified WmulW16
import qualified Wmul32
import qualified WmulW32
import qualified Wmul64
import qualified WmulW64

repeatedTest :: String -> Test -> Test
repeatedTest tname test = TestLabel tname $ TestList $ replicate 1024 test

numTests = TestList [ repeatedTest "plus" $ TestCase $ assertNum plus Wplus.plus
                    , repeatedTest "plus8" $ TestCase $ assertNum plus8 Wplus8.plus8
                    , repeatedTest "plus16" $ TestCase $ assertNum plus16 Wplus16.plus16
                    , repeatedTest "plus32" $ TestCase $ assertNum plus32 Wplus32.plus32
                    , repeatedTest "plus64" $ TestCase $ assertNum plus64 Wplus64.plus64
                    , repeatedTest "plusW" $ TestCase $ assertNum plusW WplusW.plusW
                    , repeatedTest "plusW8" $ TestCase $ assertNum plusW8 WplusW8.plusW8
                    , repeatedTest "plusW16" $ TestCase $ assertNum plusW16 WplusW16.plusW16
                    , repeatedTest "plusW32" $ TestCase $ assertNum plusW32 WplusW32.plusW32
                    , repeatedTest "plusW64" $ TestCase $ assertNum plusW64 WplusW64.plusW64
                    , repeatedTest "minus" $ TestCase $ assertNum minus Wminus.minus
                    , repeatedTest "minus8" $ TestCase $ assertNum minus8 Wminus8.minus8
                    , repeatedTest "minus16" $ TestCase $ assertNum minus16 Wminus16.minus16
                    , repeatedTest "minus32" $ TestCase $ assertNum minus32 Wminus32.minus32
                    , repeatedTest "minus64" $ TestCase $ assertNum minus64 Wminus64.minus64
                    , repeatedTest "minusW" $ TestCase $ assertNum minusW WminusW.minusW
                    , repeatedTest "minusW8" $ TestCase $ assertNum minusW8 WminusW8.minusW8
                    , repeatedTest "minusW16" $ TestCase $ assertNum minusW16 WminusW16.minusW16
                    , repeatedTest "minusW32" $ TestCase $ assertNum minusW32 WminusW32.minusW32
                    , repeatedTest "minusW64" $ TestCase $ assertNum minusW64 WminusW64.minusW64
                    , repeatedTest "mul" $ TestCase $ assertNum mul Wmul.mul
                    , repeatedTest "mul8" $ TestCase $ assertNum mul8 Wmul8.mul8
                    , repeatedTest "mul16" $ TestCase $ assertNum mul16 Wmul16.mul16
                    , repeatedTest "mul32" $ TestCase $ assertNum mul32 Wmul32.mul32
                    , repeatedTest "mul64" $ TestCase $ assertNum mul64 Wmul64.mul64
                    , repeatedTest "mulW" $ TestCase $ assertNum mul Wmul.mul
                    , repeatedTest "mulW8" $ TestCase $ assertNum mulW8 WmulW8.mulW8
                    , repeatedTest "mulW16" $ TestCase $ assertNum mulW16 WmulW16.mulW16
                    , repeatedTest "mulW32" $ TestCase $ assertNum mulW32 WmulW32.mulW32
                    , repeatedTest "mulW64" $ TestCase $ assertNum mulW64 WmulW64.mulW64
                    ]

assertNum :: (Random a, Random b, Eq c, Show c) => (a -> b -> c) -> (a -> b -> IO c) -> Assertion -- Assertion = IO ()
assertNum goldenF testF = do op0 <- randomIO
                             op1 <- randomIO
                             test <- testF op0 op1
                             assertEqual "" (goldenF op0 op1) test

main = do counts <- runTestTT numTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
