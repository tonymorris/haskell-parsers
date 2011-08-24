satisfy :: (Char -> Bool) -> Parser Char
satisfy p = bindParser character (\c -> if p c then value c else failed)

is :: Char -> Parser Char
is c = satisfy (== c)