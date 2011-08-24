digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Int
natural = mapParser (list digit) read

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many1 space

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

alpha :: Parser Char
alpha = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum