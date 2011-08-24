(>>>) :: Parser a -> Parser b -> Parser b
p >>> q = bindParser p (\_ -> q)