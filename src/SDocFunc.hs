module SDocFunc(SDocFunc(SDocFunc), SDocSeg(Body, Hole, Variadic), apply) where

import Data.Word
import Outputable

data SDocSeg = Body SDoc -- Already filled in contents
             | Hole Word8 -- Positions to be filled with SDoc
             | Variadic [SDoc] -- Variadic arguments

updateSeg :: SDoc -> SDocSeg -> SDocSeg
updateSeg arg (Hole i) = case i of
                           0 -> Body arg
                           _ -> Hole (i - 1)
updateSeg arg (Variadic args) = Variadic (args ++ [arg])
updateSeg _ seg = seg -- No change for filled segment

pprSeg :: SDocSeg -> SDoc
pprSeg (Body doc) = doc
pprSeg (Hole _) = error "Insufficient argument for SDocFunc"
pprSeg (Variadic docs) = pprWithCommas id docs

data SDocFunc = SDocFunc [SDocSeg]

-- | Apply one SDoc content as argument to a SDocFunc, so that all the
-- Hole 0 will be filled with that SDoc, all other holes decreased by
-- one.
apply :: SDocFunc -> SDoc -> SDocFunc
apply (SDocFunc segs) doc = SDocFunc $ map (updateSeg doc) segs


instance Outputable SDocFunc where
  ppr (SDocFunc segs) = cat $ map pprSeg segs
