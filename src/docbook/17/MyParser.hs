firstNameParser :: Parser String
firstNameParser = bindParser upper (\c -> mapParser (list lower) (\cs -> c : cs))