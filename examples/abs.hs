module Abs where

abs :: Word -> Word -> Word
abs a b | a < b = b - a
        | otherwise = a - b

        
