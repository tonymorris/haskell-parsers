bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser (P p) f = P (\s -> case p s of Just (r, c) -> parse (f c) r
                                          Nothing -> Nothing)