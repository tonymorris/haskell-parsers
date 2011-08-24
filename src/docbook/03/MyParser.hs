value :: a -> Parser a
value a = P (\s -> Just (s, a))