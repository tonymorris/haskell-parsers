(|||) :: Parser a -> Parser a -> Parser a
P p1 ||| P p2 = P (\s -> case p1 s of v@(Just _) -> v
                                      Nothing -> p2 s)