thisMany :: Int -> Parser a -> Parser [a]
thisMany n p = sequenceParser (replicate n p)