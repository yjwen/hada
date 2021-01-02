import Test.HUnit
import System.Random
import Control.Monad
import Num

import System.Exit

import qualified Wplus
import qualified WplusU
import qualified Wplus8
import qualified WplusU8
import qualified Wplus16
import qualified WplusU16
import qualified Wplus32
import qualified WplusU32
import qualified Wplus64
import qualified WplusU64
import qualified Wminus
import qualified WminusU
import qualified Wminus8
import qualified WminusU8
import qualified Wminus16
import qualified WminusU16
import qualified Wminus32
import qualified WminusU32
import qualified Wminus64
import qualified WminusU64
import qualified Wmul
import qualified WmulU
import qualified Wmul8
import qualified WmulU8
import qualified Wmul16
import qualified WmulU16
import qualified Wmul32
import qualified WmulU32
import qualified Wmul64
import qualified WmulU64
import qualified Wneg
import qualified Wneg8
import qualified Wneg16
import qualified Wneg32
import qualified Wneg64
import qualified WnegU
import qualified WnegU8
import qualified WnegU16
import qualified WnegU32
import qualified WnegU64
import qualified Wabs_
import qualified WabsU
import qualified Wabs8
import qualified WabsU8
import qualified Wabs16
import qualified WabsU16
import qualified Wabs32
import qualified WabsU32
import qualified Wabs64
import qualified WabsU64
import qualified Wsig
import qualified Wsig8
import qualified Wsig16
import qualified Wsig32
import qualified Wsig64
import qualified WsigU
import qualified WsigU8
import qualified WsigU16
import qualified WsigU32
import qualified WsigU64
import qualified Weq
import qualified Weq8
import qualified Weq16
import qualified Weq32
import qualified Weq64
import qualified WeqU
import qualified WeqU8
import qualified WeqU16
import qualified WeqU32
import qualified WeqU64
import qualified Wneq
import qualified Wneq8
import qualified Wneq16
import qualified Wneq32
import qualified Wneq64
import qualified WneqU
import qualified WneqU8
import qualified WneqU16
import qualified WneqU32
import qualified WneqU64
import qualified Wlt
import qualified Wlt8
import qualified Wlt16
import qualified Wlt32
import qualified Wlt64
import qualified WltU
import qualified WltU8
import qualified WltU16
import qualified WltU32
import qualified WltU64
import qualified Wle
import qualified Wle8
import qualified Wle16
import qualified Wle32
import qualified Wle64
import qualified WleU
import qualified WleU8
import qualified WleU16
import qualified WleU32
import qualified WleU64
import qualified Wgt
import qualified Wgt8
import qualified Wgt16
import qualified Wgt32
import qualified Wgt64
import qualified WgtU
import qualified WgtU8
import qualified WgtU16
import qualified WgtU32
import qualified WgtU64
import qualified Wge
import qualified Wge8
import qualified Wge16
import qualified Wge32
import qualified Wge64
import qualified WgeU
import qualified WgeU8
import qualified WgeU16
import qualified WgeU32
import qualified WgeU64
import qualified Wband
import qualified Wband8
import qualified Wband16
import qualified Wband32
import qualified Wband64
import qualified WbandU
import qualified WbandU8
import qualified WbandU16
import qualified WbandU32
import qualified WbandU64
import qualified Wbor
import qualified Wbor8
import qualified Wbor16
import qualified Wbor32
import qualified Wbor64
import qualified WborU
import qualified WborU8
import qualified WborU16
import qualified WborU32
import qualified WborU64
import qualified Wbxor
import qualified Wbxor8
import qualified Wbxor16
import qualified Wbxor32
import qualified Wbxor64
import qualified WbxorU
import qualified WbxorU8
import qualified WbxorU16
import qualified WbxorU32
import qualified WbxorU64
import qualified Wbneg
import qualified Wbneg8
import qualified Wbneg16
import qualified Wbneg32
import qualified Wbneg64
import qualified WbnegU
import qualified WbnegU8
import qualified WbnegU16
import qualified WbnegU32
import qualified WbnegU64


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
           , ("plusU" , testNum plusU WplusU.plusU)
           , ("plusU8" , testNum plusU8 WplusU8.plusU8)
           , ("plusU16" , testNum plusU16 WplusU16.plusU16)
           , ("plusU32" , testNum plusU32 WplusU32.plusU32)
           , ("plusU64" , testNum plusU64 WplusU64.plusU64)
           , ("minus" , testNum minus Wminus.minus)
           , ("minus8" , testNum minus8 Wminus8.minus8)
           , ("minus16" , testNum minus16 Wminus16.minus16)
           , ("minus32" , testNum minus32 Wminus32.minus32)
           , ("minus64" , testNum minus64 Wminus64.minus64)
           , ("minusU" , testNum minusU WminusU.minusU)
           , ("minusU8" , testNum minusU8 WminusU8.minusU8)
           , ("minusU16" , testNum minusU16 WminusU16.minusU16)
           , ("minusU32" , testNum minusU32 WminusU32.minusU32)
           , ("minusU64" , testNum minusU64 WminusU64.minusU64)
           , ("mul" , testNum mul Wmul.mul)
           , ("mul8" , testNum mul8 Wmul8.mul8)
           , ("mul16" , testNum mul16 Wmul16.mul16)
           , ("mul32" , testNum mul32 Wmul32.mul32)
           , ("mul64" , testNum mul64 Wmul64.mul64)
           , ("mulU" , testNum mulU WmulU.mulU)
           , ("mulU8" , testNum mulU8 WmulU8.mulU8)
           , ("mulU16" , testNum mulU16 WmulU16.mulU16)
           , ("mulU32" , testNum mulU32 WmulU32.mulU32)
           , ("mulU64" , testNum mulU64 WmulU64.mulU64)
           , ("neg", testNum1 neg Wneg.neg)
           , ("neg8", testNum1 neg8 Wneg8.neg8)
           , ("neg16", testNum1 neg16 Wneg16.neg16)
           , ("neg32", testNum1 neg32 Wneg32.neg32)
           , ("neg64", testNum1 neg64 Wneg64.neg64)
           , ("negU", testNum1 negU WnegU.negU)
           , ("negU8", testNum1 negU8 WnegU8.negU8)
           , ("negU16", testNum1 negU16 WnegU16.negU16)
           , ("negU32", testNum1 negU32 WnegU32.negU32)
           , ("negU64", testNum1 negU64 WnegU64.negU64)
           , ("abs", testNum1 abs_ Wabs_.abs_)
           , ("abs8", testNum1 abs8 Wabs8.abs8)
           , ("abs16", testNum1 abs16 Wabs16.abs16)
           , ("abs32", testNum1 abs32 Wabs32.abs32)
           , ("abs64", testNum1 abs64 Wabs64.abs64)
           , ("absU", testNum1 absU WabsU.absU)
           , ("absU8", testNum1 absU8 WabsU8.absU8)
           , ("absU16", testNum1 absU16 WabsU16.absU16)
           , ("absU32", testNum1 absU32 WabsU32.absU32)
           , ("absU64", testNum1 absU64 WabsU64.absU64)
           , ("sig", testNum1 sig Wsig.sig)
           , ("sig8", testNum1 sig8 Wsig8.sig8)
           , ("sig16", testNum1 sig16 Wsig16.sig16)
           , ("sig32", testNum1 sig32 Wsig32.sig32)
           , ("sig64", testNum1 sig64 Wsig64.sig64)
           , ("sigU", testNum1 sigU WsigU.sigU)
           , ("sigU8", testNum1 sigU8 WsigU8.sigU8)
           , ("sigU16", testNum1 sigU16 WsigU16.sigU16)
           , ("sigU32", testNum1 sigU32 WsigU32.sigU32)
           , ("sigU64", testNum1 sigU64 WsigU64.sigU64)
           , ("eq", testNum eq Weq.eq)
           , ("eq8", testNum eq8 Weq8.eq8)
           , ("eq16", testNum eq16 Weq16.eq16)
           , ("eq32", testNum eq32 Weq32.eq32)
           , ("eq64", testNum eq64 Weq64.eq64)
           , ("eqU", testNum eqU WeqU.eqU)
           , ("eqU8", testNum eqU8 WeqU8.eqU8)
           , ("eqU16", testNum eqU16 WeqU16.eqU16)
           , ("eqU32", testNum eqU32 WeqU32.eqU32)
           , ("eqU64", testNum eqU64 WeqU64.eqU64)
           , ("neq", testNum neq Wneq.neq)
           , ("neq8", testNum neq8 Wneq8.neq8)
           , ("neq16", testNum neq16 Wneq16.neq16)
           , ("neq32", testNum neq32 Wneq32.neq32)
           , ("neq64", testNum neq64 Wneq64.neq64)
           , ("neqU", testNum neqU WneqU.neqU)
           , ("neqU8", testNum neqU8 WneqU8.neqU8)
           , ("neqU16", testNum neqU16 WneqU16.neqU16)
           , ("neqU32", testNum neqU32 WneqU32.neqU32)
           , ("neqU64", testNum neqU64 WneqU64.neqU64)
           , ("lt", testNum lt Wlt.lt)
           , ("lt8", testNum lt8 Wlt8.lt8)
           , ("lt16", testNum lt16 Wlt16.lt16)
           , ("lt32", testNum lt32 Wlt32.lt32)
           , ("lt64", testNum lt64 Wlt64.lt64)
           , ("ltU", testNum ltU WltU.ltU)
           , ("ltU8", testNum ltU8 WltU8.ltU8)
           , ("ltU16", testNum ltU16 WltU16.ltU16)
           , ("ltU32", testNum ltU32 WltU32.ltU32)
           , ("ltU64", testNum ltU64 WltU64.ltU64)
           , ("le", testNum le Wle.le)
           , ("le8", testNum le8 Wle8.le8)
           , ("le16", testNum le16 Wle16.le16)
           , ("le32", testNum le32 Wle32.le32)
           , ("le64", testNum le64 Wle64.le64)
           , ("leU", testNum leU WleU.leU)
           , ("leU8", testNum leU8 WleU8.leU8)
           , ("leU16", testNum leU16 WleU16.leU16)
           , ("leU32", testNum leU32 WleU32.leU32)
           , ("leU64", testNum leU64 WleU64.leU64)
           , ("gt", testNum gt Wgt.gt)
           , ("gt8", testNum gt8 Wgt8.gt8)
           , ("gt16", testNum gt16 Wgt16.gt16)
           , ("gt32", testNum gt32 Wgt32.gt32)
           , ("gt64", testNum gt64 Wgt64.gt64)
           , ("gtU", testNum gtU WgtU.gtU)
           , ("gtU8", testNum gtU8 WgtU8.gtU8)
           , ("gtU16", testNum gtU16 WgtU16.gtU16)
           , ("gtU32", testNum gtU32 WgtU32.gtU32)
           , ("gtU64", testNum gtU64 WgtU64.gtU64)
           , ("ge", testNum ge Wge.ge)
           , ("ge8", testNum ge8 Wge8.ge8)
           , ("ge16", testNum ge16 Wge16.ge16)
           , ("ge32", testNum ge32 Wge32.ge32)
           , ("ge64", testNum ge64 Wge64.ge64)
           , ("geU", testNum geU WgeU.geU)
           , ("geU8", testNum geU8 WgeU8.geU8)
           , ("geU16", testNum geU16 WgeU16.geU16)
           , ("geU32", testNum geU32 WgeU32.geU32)
           , ("geU64", testNum geU64 WgeU64.geU64)
           , ("band", testNum band Wband.band)
           , ("band8", testNum band8 Wband8.band8)
           , ("band16", testNum band16 Wband16.band16)
           , ("band32", testNum band32 Wband32.band32)
           , ("band64", testNum band64 Wband64.band64)
           , ("bandU", testNum bandU WbandU.bandU)
           , ("bandU8", testNum bandU8 WbandU8.bandU8)
           , ("bandU16", testNum bandU16 WbandU16.bandU16)
           , ("bandU32", testNum bandU32 WbandU32.bandU32)
           , ("bandU64", testNum bandU64 WbandU64.bandU64)
           , ("bor", testNum bor Wbor.bor)
           , ("bor8", testNum bor8 Wbor8.bor8)
           , ("bor16", testNum bor16 Wbor16.bor16)
           , ("bor32", testNum bor32 Wbor32.bor32)
           , ("bor64", testNum bor64 Wbor64.bor64)
           , ("borU", testNum borU WborU.borU)
           , ("borU8", testNum borU8 WborU8.borU8)
           , ("borU16", testNum borU16 WborU16.borU16)
           , ("borU32", testNum borU32 WborU32.borU32)
           , ("borU64", testNum borU64 WborU64.borU64)
           , ("bxor", testNum bxor Wbxor.bxor)
           , ("bxor8", testNum bxor8 Wbxor8.bxor8)
           , ("bxor16", testNum bxor16 Wbxor16.bxor16)
           , ("bxor32", testNum bxor32 Wbxor32.bxor32)
           , ("bxor64", testNum bxor64 Wbxor64.bxor64)
           , ("bxorU", testNum bxorU WbxorU.bxorU)
           , ("bxorU8", testNum bxorU8 WbxorU8.bxorU8)
           , ("bxorU16", testNum bxorU16 WbxorU16.bxorU16)
           , ("bxorU32", testNum bxorU32 WbxorU32.bxorU32)
           , ("bxorU64", testNum bxorU64 WbxorU64.bxorU64)
           , ("bneg", testNum1 bneg Wbneg.bneg)
           , ("bneg8", testNum1 bneg8 Wbneg8.bneg8)
           , ("bneg16", testNum1 bneg16 Wbneg16.bneg16)
           , ("bneg32", testNum1 bneg32 Wbneg32.bneg32)
           , ("bneg64", testNum1 bneg64 Wbneg64.bneg64)
           , ("bnegU", testNum1 bnegU WbnegU.bnegU)
           , ("bnegU8", testNum1 bnegU8 WbnegU8.bnegU8)
           , ("bnegU16", testNum1 bnegU16 WbnegU16.bnegU16)
           , ("bnegU32", testNum1 bnegU32 WbnegU32.bnegU32)
           , ("bnegU64", testNum1 bnegU64 WbnegU64.bnegU64)
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

-- | Test unary function
testNum1 :: (Random a, Show a, Eq b, Show b) => (a -> b) -> (a -> IO b) -> IO (Maybe String)
testNum1 goldenF testF = do op <- randomIO
                            test <- testF op
                            let golden = goldenF op
                            return (if test == golden
                                     then Nothing
                                     else Just ("Expecting " ++ show golden ++
                                                ", but found " ++ show test ++
                                                ". Operand is " ++ show op ++ "."))
                                     

main = do counts <- runTestTT numTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
