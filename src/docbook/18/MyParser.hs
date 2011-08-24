surnameParser :: Parser String
surnameParser = bindParser upper (\c -> bindParser (thisMany 5 lower) (\cs -> mapParser (list lower) (\t -> c : cs ++ t)))