module MaybePlus where
maybeplus :: Maybe Int -> Maybe Int -> Maybe Int
maybeplus (Just a) (Just b) = Just (a + b)
maybeplus _ _ = Nothing

