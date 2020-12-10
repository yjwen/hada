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

repeatedTest :: IO (Maybe String)  -> Test
repeatedTest test = TestCase ((sequence $ replicate 1024 test) >>= assertAllNothing)
  where assertAllNothing [] = return ()
        assertAllNothing (x:xs) | Just msg <- x = assertFailure msg
                                | Nothing  <- x = assertAllNothing xs

numTests = TestList [ TestLabel "plus" $ repeatedTest $ testNum plus Wplus.plus
                    , TestLabel "plus8" $ repeatedTest $ testNum plus8 Wplus8.plus8
                    , TestLabel "plus16" $ repeatedTest $ testNum plus16 Wplus16.plus16
                    , TestLabel "plus32" $ repeatedTest $ testNum plus32 Wplus32.plus32
                    , TestLabel "plus64" $ repeatedTest $ testNum plus64 Wplus64.plus64
                    , TestLabel "plusW" $ repeatedTest $ testNum plusW WplusW.plusW
                    , TestLabel "plusW8" $ repeatedTest $ testNum plusW8 WplusW8.plusW8
                    , TestLabel "plusW16" $ repeatedTest $ testNum plusW16 WplusW16.plusW16
                    , TestLabel "plusW32" $ repeatedTest $ testNum plusW32 WplusW32.plusW32
                    , TestLabel "plusW64" $ repeatedTest $ testNum plusW64 WplusW64.plusW64
                    , TestLabel "minus" $ repeatedTest $ testNum minus Wminus.minus
                    , TestLabel "minus8" $ repeatedTest $ testNum minus8 Wminus8.minus8
                    , TestLabel "minus16" $ repeatedTest $ testNum minus16 Wminus16.minus16
                    , TestLabel "minus32" $ repeatedTest $ testNum minus32 Wminus32.minus32
                    , TestLabel "minus64" $ repeatedTest $ testNum minus64 Wminus64.minus64
                    , TestLabel "minusW" $ repeatedTest $ testNum minusW WminusW.minusW
                    , TestLabel "minusW8" $ repeatedTest $ testNum minusW8 WminusW8.minusW8
                    , TestLabel "minusW16" $ repeatedTest $ testNum minusW16 WminusW16.minusW16
                    , TestLabel "minusW32" $ repeatedTest $ testNum minusW32 WminusW32.minusW32
                    , TestLabel "minusW64" $ repeatedTest $ testNum minusW64 WminusW64.minusW64
                    , TestLabel "mul" $ repeatedTest $ testNum mul Wmul.mul
                    , TestLabel "mul8" $ repeatedTest $ testNum mul8 Wmul8.mul8
                    , TestLabel "mul16" $ repeatedTest $ testNum mul16 Wmul16.mul16
                    , TestLabel "mul32" $ repeatedTest $ testNum mul32 Wmul32.mul32
                    , TestLabel "mul64" $ repeatedTest $ testNum mul64 Wmul64.mul64
                    , TestLabel "mulW" $ repeatedTest $ testNum mul Wmul.mul
                    , TestLabel "mulW8" $ repeatedTest $ testNum mulW8 WmulW8.mulW8
                    , TestLabel "mulW16" $ repeatedTest $ testNum mulW16 WmulW16.mulW16
                    , TestLabel "mulW32" $ repeatedTest $ testNum mulW32 WmulW32.mulW32
                    , TestLabel "mulW64" $ repeatedTest $ testNum mulW64 WmulW64.mulW64
                    ]

-- | Compare verilated binary function with its origin, return Nothing
-- at success, or a description at a failure
testNum :: (Random a, Show a,
            Random b, Show b,
            Eq c, Show c) =>
           (a -> b -> c) -> (a -> b -> IO c) -> IO (Maybe String)
testNum goldenF testF = do op0 <- randomIO
                           op1 <- randomIO
                           test <- testF op0 op1
                           let golden = goldenF op0 op1
                           return (if test == golden
                                   then Nothing
                                   else Just ("Expecting " ++ show golden ++
                                              ", but found " ++ show test ++
                                              ". Operands are (" ++ show op0 ++
                                              ", " ++ show op1 ++
                                              ")."))
main = do counts <- runTestTT numTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
