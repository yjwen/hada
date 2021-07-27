-- A light-weight structured text with horizontal and vertical
-- concatenation. Eq instance provided, so that it can be used as
-- TopoGraph node.
module VHText ( VHText(..)
                -- Empty aware combinators
              , vcat, hcat, vcatAll, hcatAll
              , indent
                -- Interspersing
              , leftIntersperse, rightIntersperse
              , rightSep) where

import Outputable( Outputable, text, empty
                 , ($$), (<>), SDoc, ppr)

data VHText = VCat VHText VHText
              -- Vertical concatenation of two texts
            | HCat VHText VHText
              -- Horizontal concatenation of two texts
            | Text String
            | Empty
            deriving (Eq)

instance Outputable VHText where
  ppr (VCat upper lower) = ppr upper $$ ppr lower
  ppr (HCat left right) = ppr left Outputable.<> ppr right
  ppr (Text s) = text s
  ppr Empty = empty

-- Emptiness-aware catenations
vcat :: VHText -> VHText -> VHText
vcat Empty a = a
vcat a Empty = a
vcat a b = VCat a b

hcat :: VHText -> VHText -> VHText
hcat Empty a = a
hcat a Empty = a
hcat a b = HCat a b

vcatAll :: [VHText] -> VHText
vcatAll = foldl vcat Empty

hcatAll :: [VHText] -> VHText
hcatAll = foldl hcat Empty

-- rightIntersperse a txts inserts a between every VCat and HCat block
-- of txts by right-attaching a to the left/upper block
rightIntersperse :: VHText -> VHText -> VHText
rightIntersperse a (VCat x y) = VCat (HCat (rightIntersperse a x) a) (rightIntersperse a y)
rightIntersperse a (HCat x y) = HCat (HCat (rightIntersperse a x) a) (rightIntersperse a y)
rightIntersperse _ txts = txts

-- leftIntersperse a txts inserts a between every VCat and HCat block
-- of txts by left-attaching a to the right/lower block
leftIntersperse :: VHText -> VHText -> VHText
leftIntersperse a (VCat x y) = VCat (leftIntersperse a x) (HCat a (leftIntersperse a y))
leftIntersperse a (HCat x y) = HCat (leftIntersperse a x) (HCat a (leftIntersperse a y))


rightSep :: String -> VHText -> VHText
rightSep = rightIntersperse . Text

indent :: Int -> VHText -> VHText
indent i = (Text (replicate i ' ') `hcat`)
