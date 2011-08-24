mapParser :: Parser a -> (a -> b) -> Parser b
mapParser (P p) f = P (\s -> case p s of Just (r, c) -> Just (r, f c)
                                         Nothing -> Nothing)