-- More functions on List
module ListX (decap, decapAny) where

-- | decap a b = Just c, where b = a ++ c if any, otherwise nothing
decap :: (Eq a) => [a] -> [a] -> Maybe [a]
decap (a:as) (b:bs) = case a == b of
                        True ->  decap as bs
                        False -> Nothing
decap [] bs = Just bs
decap as [] = Nothing

-- | decapAny A b = Just (a, c) when the first a∈A that satisfies b =
-- a ++ c is found, otherwise return Nothing
decapAny :: (Eq a) => [[a]] -> [a] -> Maybe ([a], [a])
decapAny (a:as) b
  | Just c <- decap a b = Just (a, c)
  | otherwise = decapAny as b
decapAny [] b = Nothing
