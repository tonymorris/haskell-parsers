phoneBodyParser :: Parser String
phoneBodyParser = list (digit ||| is '.' ||| is '-')