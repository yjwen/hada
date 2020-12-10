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

numTests = (TestList . map (\(name, test) -> TestLabel name $ repeatedTest test))
           [ ("plus" , testNum plus Wplus.plus)
           , ("plus8" , testNum plus8 Wplus8.plus8)
           , ("plus16" , testNum plus16 Wplus16.plus16)
           , ("plus32" , testNum plus32 Wplus32.plus32)
           , ("plus64" , testNum plus64 Wplus64.plus64)
           , ("plusW" , testNum plusW WplusW.plusW)
           , ("plusW8" , testNum plusW8 WplusW8.plusW8)
           , ("plusW16" , testNum plusW16 WplusW16.plusW16)
           , ("plusW32" , testNum plusW32 WplusW32.plusW32)
           , ("plusW64" , testNum plusW64 WplusW64.plusW64)
           , ("minus" , testNum minus Wminus.minus)
           , ("minus8" , testNum minus8 Wminus8.minus8)
           , ("minus16" , testNum minus16 Wminus16.minus16)
           , ("minus32" , testNum minus32 Wminus32.minus32)
           , ("minus64" , testNum minus64 Wminus64.minus64)
           , ("minusW" , testNum minusW WminusW.minusW)
           , ("minusW8" , testNum minusW8 WminusW8.minusW8)
           , ("minusW16" , testNum minusW16 WminusW16.minusW16)
           , ("minusW32" , testNum minusW32 WminusW32.minusW32)
           , ("minusW64" , testNum minusW64 WminusW64.minusW64)
           , ("mul" , testNum mul Wmul.mul)
           , ("mul8" , testNum mul8 Wmul8.mul8)
           , ("mul16" , testNum mul16 Wmul16.mul16)
           , ("mul32" , testNum mul32 Wmul32.mul32)
           , ("mul64" , testNum mul64 Wmul64.mul64)
           , ("mulW" , testNum mul Wmul.mul)
           , ("mulW8" , testNum mulW8 WmulW8.mulW8)
           , ("mulW16" , testNum mulW16 WmulW16.mulW16)
           , ("mulW32" , testNum mulW32 WmulW32.mulW32)
           , ("mulW64" , testNum mulW64 WmulW64.mulW64)
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
