-- | More functions on List
module ListX (stripAnyPrefix) where
import Data.List(stripPrefix)

-- | stripAnyPrefix A b = Just (a, c) when the first a∈A that satisfies b =
-- a ++ c is found, otherwise return Nothing
stripAnyPrefix :: (Eq a) => [[a]] -> [a] -> Maybe ([a], [a])
stripAnyPrefix (a:as) b
  | Just c <- stripPrefix a b = Just (a, c)
  | otherwise = stripAnyPrefix as b
stripAnyPrefix [] _ = Nothing
