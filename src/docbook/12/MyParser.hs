list :: Parser a -> Parser [a]
list k = many1 k ||| value []

many1 :: Parser a -> Parser [a]
many1 k = bindParser k (\k' -> mapParser (list k) (\kk' -> k' : kk'))