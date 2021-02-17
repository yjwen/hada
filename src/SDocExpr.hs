module SDocExpr( SDocExpr(SDocFunc, SDocConst)
               , SDocSeg(Body, Hole, Variadic)
               , apply
               , binarySDocFunc, unarySDocFunc, bypassSDocFunc, funCallSDocFunc
               ) where
import Prelude hiding ((<>))
import Data.Word
import Outputable

data SDocSeg = Body SDoc
             -- Already filled in contents
             | Hole Word8
             -- Positions to be filled with SDoc. Hole n indicates it
             -- is for the n-th argument.
             | ConstHole Word8
             -- Positions to be filled by a constant expression, aka,
             -- SDocConst. ConstHole is for restriction of
             -- synthesizable constructs, like integers shifting must
             -- be by a constant value to be synthesizable.
             | Variadic [SDoc] -- Variadic arguments

data SDocExpr = SDocFunc [SDocSeg]
              | SDocConst SDoc

instance Outputable SDocExpr where
  ppr (SDocFunc segs) = cat $ map pprSeg segs
  ppr (SDocConst sdoc) = sdoc

updateSeg :: SDocExpr -> SDocSeg -> SDocSeg
updateSeg expr (Hole i) = case i of
                           0 -> Body $ ppr expr
                           _ -> Hole (i - 1)
updateSeg expr (ConstHole i) = case i of
                                 0 -> case expr of
                                        SDocFunc _ -> error "Expecting a constant."
                                        SDocConst sdoc -> Body sdoc
                                 _ -> ConstHole (i - 1)
updateSeg expr (Variadic args) = Variadic (args ++ [ppr expr])
updateSeg _ sdocf = sdocf -- No change on Body

pprSeg :: SDocSeg -> SDoc
pprSeg (Body doc) = doc
pprSeg (Variadic docs) = pprWithCommas id docs
pprSeg _ = error "Insufficient argument" -- For Hole and ConstHole

-- | Apply one SDocExpr as argument to another SDocExpr, so that all
-- the Hole 0 or ConstHole 0 will be filled with that SDoc, all other
-- holes decreased by one.
apply :: SDocExpr -> SDocExpr -> SDocExpr
apply (SDocFunc segs) expr = SDocFunc $ map (updateSeg expr) segs
apply (SDocConst _) _ = error "Applying argument to a constant"

binarySDocFunc :: String -> SDocExpr
binarySDocFunc op = SDocFunc ((Hole 0) : (Body (space <> text op <> space)) : (Hole 1) : [])

unarySDocFunc :: String -> SDocExpr
unarySDocFunc op = SDocFunc ((Body (text op)) : (Hole 0) : [])

bypassSDocFunc :: SDocExpr
bypassSDocFunc = SDocFunc (Hole 0 : [])

funCallSDocFunc :: String -> SDocExpr
funCallSDocFunc fName = SDocFunc (Body (text fName <> lparen) : Variadic [] : Body rparen : [])

