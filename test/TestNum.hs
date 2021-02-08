import Test.HUnit
import Control.Monad
import Num
import Test

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


allTests = (TestList . map (\(name, test) -> TestLabel name $ repeatedTest 1024 test))
           [ ("plus" , test2 plus Wplus.plus)
           , ("plus8" , test2 plus8 Wplus8.plus8)
           , ("plus16" , test2 plus16 Wplus16.plus16)
           , ("plus32" , test2 plus32 Wplus32.plus32)
           , ("plus64" , test2 plus64 Wplus64.plus64)
           , ("plusU" , test2 plusU WplusU.plusU)
           , ("plusU8" , test2 plusU8 WplusU8.plusU8)
           , ("plusU16" , test2 plusU16 WplusU16.plusU16)
           , ("plusU32" , test2 plusU32 WplusU32.plusU32)
           , ("plusU64" , test2 plusU64 WplusU64.plusU64)
           , ("minus" , test2 minus Wminus.minus)
           , ("minus8" , test2 minus8 Wminus8.minus8)
           , ("minus16" , test2 minus16 Wminus16.minus16)
           , ("minus32" , test2 minus32 Wminus32.minus32)
           , ("minus64" , test2 minus64 Wminus64.minus64)
           , ("minusU" , test2 minusU WminusU.minusU)
           , ("minusU8" , test2 minusU8 WminusU8.minusU8)
           , ("minusU16" , test2 minusU16 WminusU16.minusU16)
           , ("minusU32" , test2 minusU32 WminusU32.minusU32)
           , ("minusU64" , test2 minusU64 WminusU64.minusU64)
           , ("mul" , test2 mul Wmul.mul)
           , ("mul8" , test2 mul8 Wmul8.mul8)
           , ("mul16" , test2 mul16 Wmul16.mul16)
           , ("mul32" , test2 mul32 Wmul32.mul32)
           , ("mul64" , test2 mul64 Wmul64.mul64)
           , ("mulU" , test2 mulU WmulU.mulU)
           , ("mulU8" , test2 mulU8 WmulU8.mulU8)
           , ("mulU16" , test2 mulU16 WmulU16.mulU16)
           , ("mulU32" , test2 mulU32 WmulU32.mulU32)
           , ("mulU64" , test2 mulU64 WmulU64.mulU64)
           , ("neg", test1 neg Wneg.neg)
           , ("neg8", test1 neg8 Wneg8.neg8)
           , ("neg16", test1 neg16 Wneg16.neg16)
           , ("neg32", test1 neg32 Wneg32.neg32)
           , ("neg64", test1 neg64 Wneg64.neg64)
           , ("negU", test1 negU WnegU.negU)
           , ("negU8", test1 negU8 WnegU8.negU8)
           , ("negU16", test1 negU16 WnegU16.negU16)
           , ("negU32", test1 negU32 WnegU32.negU32)
           , ("negU64", test1 negU64 WnegU64.negU64)
           , ("abs", test1 abs_ Wabs_.abs_)
           , ("abs8", test1 abs8 Wabs8.abs8)
           , ("abs16", test1 abs16 Wabs16.abs16)
           , ("abs32", test1 abs32 Wabs32.abs32)
           , ("abs64", test1 abs64 Wabs64.abs64)
           , ("absU", test1 absU WabsU.absU)
           , ("absU8", test1 absU8 WabsU8.absU8)
           , ("absU16", test1 absU16 WabsU16.absU16)
           , ("absU32", test1 absU32 WabsU32.absU32)
           , ("absU64", test1 absU64 WabsU64.absU64)
           , ("sig", test1 sig Wsig.sig)
           , ("sig8", test1 sig8 Wsig8.sig8)
           , ("sig16", test1 sig16 Wsig16.sig16)
           , ("sig32", test1 sig32 Wsig32.sig32)
           , ("sig64", test1 sig64 Wsig64.sig64)
           , ("sigU", test1 sigU WsigU.sigU)
           , ("sigU8", test1 sigU8 WsigU8.sigU8)
           , ("sigU16", test1 sigU16 WsigU16.sigU16)
           , ("sigU32", test1 sigU32 WsigU32.sigU32)
           , ("sigU64", test1 sigU64 WsigU64.sigU64)
           , ("eq", test2 eq Weq.eq)
           , ("eq8", test2 eq8 Weq8.eq8)
           , ("eq16", test2 eq16 Weq16.eq16)
           , ("eq32", test2 eq32 Weq32.eq32)
           , ("eq64", test2 eq64 Weq64.eq64)
           , ("eqU", test2 eqU WeqU.eqU)
           , ("eqU8", test2 eqU8 WeqU8.eqU8)
           , ("eqU16", test2 eqU16 WeqU16.eqU16)
           , ("eqU32", test2 eqU32 WeqU32.eqU32)
           , ("eqU64", test2 eqU64 WeqU64.eqU64)
           , ("neq", test2 neq Wneq.neq)
           , ("neq8", test2 neq8 Wneq8.neq8)
           , ("neq16", test2 neq16 Wneq16.neq16)
           , ("neq32", test2 neq32 Wneq32.neq32)
           , ("neq64", test2 neq64 Wneq64.neq64)
           , ("neqU", test2 neqU WneqU.neqU)
           , ("neqU8", test2 neqU8 WneqU8.neqU8)
           , ("neqU16", test2 neqU16 WneqU16.neqU16)
           , ("neqU32", test2 neqU32 WneqU32.neqU32)
           , ("neqU64", test2 neqU64 WneqU64.neqU64)
           , ("lt", test2 lt Wlt.lt)
           , ("lt8", test2 lt8 Wlt8.lt8)
           , ("lt16", test2 lt16 Wlt16.lt16)
           , ("lt32", test2 lt32 Wlt32.lt32)
           , ("lt64", test2 lt64 Wlt64.lt64)
           , ("ltU", test2 ltU WltU.ltU)
           , ("ltU8", test2 ltU8 WltU8.ltU8)
           , ("ltU16", test2 ltU16 WltU16.ltU16)
           , ("ltU32", test2 ltU32 WltU32.ltU32)
           , ("ltU64", test2 ltU64 WltU64.ltU64)
           , ("le", test2 le Wle.le)
           , ("le8", test2 le8 Wle8.le8)
           , ("le16", test2 le16 Wle16.le16)
           , ("le32", test2 le32 Wle32.le32)
           , ("le64", test2 le64 Wle64.le64)
           , ("leU", test2 leU WleU.leU)
           , ("leU8", test2 leU8 WleU8.leU8)
           , ("leU16", test2 leU16 WleU16.leU16)
           , ("leU32", test2 leU32 WleU32.leU32)
           , ("leU64", test2 leU64 WleU64.leU64)
           , ("gt", test2 gt Wgt.gt)
           , ("gt8", test2 gt8 Wgt8.gt8)
           , ("gt16", test2 gt16 Wgt16.gt16)
           , ("gt32", test2 gt32 Wgt32.gt32)
           , ("gt64", test2 gt64 Wgt64.gt64)
           , ("gtU", test2 gtU WgtU.gtU)
           , ("gtU8", test2 gtU8 WgtU8.gtU8)
           , ("gtU16", test2 gtU16 WgtU16.gtU16)
           , ("gtU32", test2 gtU32 WgtU32.gtU32)
           , ("gtU64", test2 gtU64 WgtU64.gtU64)
           , ("ge", test2 ge Wge.ge)
           , ("ge8", test2 ge8 Wge8.ge8)
           , ("ge16", test2 ge16 Wge16.ge16)
           , ("ge32", test2 ge32 Wge32.ge32)
           , ("ge64", test2 ge64 Wge64.ge64)
           , ("geU", test2 geU WgeU.geU)
           , ("geU8", test2 geU8 WgeU8.geU8)
           , ("geU16", test2 geU16 WgeU16.geU16)
           , ("geU32", test2 geU32 WgeU32.geU32)
           , ("geU64", test2 geU64 WgeU64.geU64)
           , ("band", test2 band Wband.band)
           , ("band8", test2 band8 Wband8.band8)
           , ("band16", test2 band16 Wband16.band16)
           , ("band32", test2 band32 Wband32.band32)
           , ("band64", test2 band64 Wband64.band64)
           , ("bandU", test2 bandU WbandU.bandU)
           , ("bandU8", test2 bandU8 WbandU8.bandU8)
           , ("bandU16", test2 bandU16 WbandU16.bandU16)
           , ("bandU32", test2 bandU32 WbandU32.bandU32)
           , ("bandU64", test2 bandU64 WbandU64.bandU64)
           , ("bor", test2 bor Wbor.bor)
           , ("bor8", test2 bor8 Wbor8.bor8)
           , ("bor16", test2 bor16 Wbor16.bor16)
           , ("bor32", test2 bor32 Wbor32.bor32)
           , ("bor64", test2 bor64 Wbor64.bor64)
           , ("borU", test2 borU WborU.borU)
           , ("borU8", test2 borU8 WborU8.borU8)
           , ("borU16", test2 borU16 WborU16.borU16)
           , ("borU32", test2 borU32 WborU32.borU32)
           , ("borU64", test2 borU64 WborU64.borU64)
           , ("bxor", test2 bxor Wbxor.bxor)
           , ("bxor8", test2 bxor8 Wbxor8.bxor8)
           , ("bxor16", test2 bxor16 Wbxor16.bxor16)
           , ("bxor32", test2 bxor32 Wbxor32.bxor32)
           , ("bxor64", test2 bxor64 Wbxor64.bxor64)
           , ("bxorU", test2 bxorU WbxorU.bxorU)
           , ("bxorU8", test2 bxorU8 WbxorU8.bxorU8)
           , ("bxorU16", test2 bxorU16 WbxorU16.bxorU16)
           , ("bxorU32", test2 bxorU32 WbxorU32.bxorU32)
           , ("bxorU64", test2 bxorU64 WbxorU64.bxorU64)
           , ("bneg", test1 bneg Wbneg.bneg)
           , ("bneg8", test1 bneg8 Wbneg8.bneg8)
           , ("bneg16", test1 bneg16 Wbneg16.bneg16)
           , ("bneg32", test1 bneg32 Wbneg32.bneg32)
           , ("bneg64", test1 bneg64 Wbneg64.bneg64)
           , ("bnegU", test1 bnegU WbnegU.bnegU)
           , ("bnegU8", test1 bnegU8 WbnegU8.bnegU8)
           , ("bnegU16", test1 bnegU16 WbnegU16.bnegU16)
           , ("bnegU32", test1 bnegU32 WbnegU32.bnegU32)
           , ("bnegU64", test1 bnegU64 WbnegU64.bnegU64)
           ]

main = do counts <- runTestTT allTests
          if errors counts > 0 || failures counts > 0
            then exitFailure
            else exitSuccess
