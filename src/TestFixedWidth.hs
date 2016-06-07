{-# LANGUAGE TemplateHaskell #-}
module Main where

import FixedWidth

$(declareFW "Bit7" "Bit7" 7)
-- data Bit7 = Bit7 Int

getValue (Bit7 a) = a
main = do print $ getValue $ Bit7 7
