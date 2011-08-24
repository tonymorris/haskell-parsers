sequenceParser :: [Parser a] -> Parser [a]
sequenceParser [] = value []
sequenceParser (h:t) = bindParser h (\a -> mapParser (sequenceParser t) (\as -> a : as))