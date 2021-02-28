module SDocExpr( SDocExpr(SDocFunc, SDocConst, SDocIdentity)
               , SDocSeg(Body, Hole, Variadic)
               , apply
               , binarySDocFunc, unarySDocFunc, funCallSDocFunc
               , binarySemiConst, literalSDocFunc
               ) where
import Prelude hiding ((<>))
import Data.Word
import Data.Int
import Outputable

data SDocSeg = Body SDoc
             -- Already filled in contents
             | Hole Word8 Int8
             -- Positions to be filled with SDoc, with
             -- precedence. Hole n p indicates it is for the n-th
             -- argument with precedence p. Larger p means higher
             -- precedence. When the hole is filled by an expression
             -- with lower precedence, the expression is encapsulated
             -- in parentheses.
             | ConstHole Word8
             -- Positions to be filled by a constant expression, aka,
             -- SDocConst. ConstHole is for restriction of
             -- synthesizable constructs, like integers shifting must
             -- be by a constant value to be synthesizable.
             | Variadic [SDoc] -- Variadic arguments

data SDocExpr = SDocFunc Int8 [SDocSeg]
              -- An expression with precedence
              | SDocConst SDoc
              -- Constant expression, assuming the SDoc is just a
              -- literal
              | SDocIdentity
              -- Applying SDocIdentity with an SDocExpr return that
              -- SDocExpr

instance Outputable SDocExpr where
  ppr (SDocFunc _ segs) = hcat $ map pprSeg segs
  ppr (SDocConst sdoc) = sdoc

updateSeg :: SDocExpr -> SDocSeg -> SDocSeg
updateSeg expr (Hole i p)
  | 0 <- i
  = case expr of
      SDocFunc ep _ -> Body $ (if ep >= p then id else parens) $ ppr expr
      SDocConst _ -> Body $ ppr expr
  | otherwise
  = Hole (i - 1) p
updateSeg expr (ConstHole i) = case i of
                                 0 -> case expr of
                                        SDocFunc _ _ -> error "Expecting a constant."
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
apply (SDocFunc p segs) expr = SDocFunc p $ map (updateSeg expr) segs
apply (SDocConst _) _ = error "Applying argument to a constant"
apply SDocIdentity expr = expr

binarySDocFunc :: String -> Int8 -> SDocExpr
binarySDocFunc op p = SDocFunc p ((Hole 0 p) : (Body (space <> text op <> space)) : (Hole 1 (p + 1)) : [])

unarySDocFunc :: String -> Int8 -> SDocExpr
unarySDocFunc op p = SDocFunc p ((Body (text op)) : (Hole 0 p) : [])

funCallSDocFunc :: String -> SDocExpr
funCallSDocFunc fName = SDocFunc maxBound (Body (text fName <> lparen) : Variadic [] : Body rparen : [])

binarySemiConst :: String -> Int8 -> SDocExpr
binarySemiConst op p = SDocFunc p ((Hole 0 p) : (Body (space <> text op <> space)) : (ConstHole 1) : [])

literalSDocFunc :: SDoc -> SDocExpr
literalSDocFunc lit = SDocFunc maxBound [Body lit]
