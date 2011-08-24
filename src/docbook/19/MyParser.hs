genderParser :: Parser Char
genderParser = is 'm' ||| is 'f'