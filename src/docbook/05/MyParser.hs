character :: Parser Char
character = P (\s -> case s of [] -> Nothing
                               (c:r) -> Just (r, c))